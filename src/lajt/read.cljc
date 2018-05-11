(ns lajt.read
  (:require
    [lajt.read.ops :as ops]
    [clojure.spec.alpha :as s]))

(def default-ops
  {:pre     [:case :depends-on :params :before]
   :actions [:query :lookup-ref :custom :no-op]
   :post    [:sort ::ops/pull :lastly]})

(defn- validate-read! [{:keys [read-map read-key] :as env}]
  (when (nil? read-map)
    (throw (ex-info (str "No such read: " read-key)
                    {:read-key read-key
                     :query    (:query env)}))))

(defn- action-error [env actions]
  (throw
    (ex-info
      (if (zero? (count actions))
        (let [registered-actions (->> (:operations env)
                                      (filter #(some #{:lajt.op.stage/action}
                                                     (map :lajt.op.stage/id
                                                          (:stages %))))
                                      (vec))]
          (str "read-map did not contain any of the actions."
              " It must contain one of: "
              registered-actions
               " had :read-map "
               (keys (:read-map env))))
        (str "Cannot have multiple actions"
             " in the read's map. Had actions: "
             actions))
      (assoc
        (select-keys env [:read-key :read-map :query])
        :actions actions))))

(defn- get-action [{:keys [read-map read-key read-ops] :as env}]
  (let [actions (filterv (set (:actions read-ops)) (keys read-map))]
    (if (== 1 (count actions))
      (first actions)
      (throw
        (ex-info
          (if (zero? (count actions))
            (str "read-map did not contain any of the actions."
                 " It must contain one of: "
                 (:actions read-ops)
                 " had: "
                 (keys read-map))
            (str "Cannot have multiple actions"
                 " in the read's map. Had actions: "
                 actions))
          {:actions  (:actions read-ops)
           :read-key read-key
           :read-map read-map
           :query    (:query env)})))))

(defn- perform-op [{:keys [read-map] :as env} read-op]
  (cond-> env
          (contains? read-map read-op)
          (ops/call read-op (get read-map read-op))))

(defn- perform-ops [env ops post-call-fn]
  (reduce (fn [env op]
            (post-call-fn env (perform-op env op)))
          env
          ops))

(defn- perform-pre-ops [{:keys [read-ops] :as env}]
  (let [env (perform-ops env
                         (:pre read-ops)
                         (fn [env after]
                           (if (= (:read-map env) (:read-map after))
                             after
                             (reduced {::new-map after}))))]
    ;; Check if :pre-ops changed the read-map
    (if-some [nm (::new-map env)]
      (recur nm)
      env)))

(defn- perform-post-ops [{:keys [read-ops] :as env}]
  (let [assoc-result #(assoc % :result (ops/get-result %))]
    (perform-ops (assoc-result env)
                 (:post read-ops)
                 (fn [_ after]
                   (assoc-result after)))))

(defn perform-read [env]
  (let [env (perform-pre-ops env)
        ;; Adds read-map with an additional ::ops/pull action
        ;; whenever there's a query.
        env (cond-> env
                    (not-empty (:query env))
                    (assoc-in [:read-map ::ops/pull] (:query env)))
        ;; Perform the action
        env (perform-op env (get-action env))]
    (perform-post-ops env)))

(def check-for-read-map-modification
  (fn [before after]
    (if (= (:read-map before) (:read-map after))
      after
      (reduced {::new-env after}))))

(defn ops-to-run [env]
  (ops/operation-order
    (ops/select-operations
      (set (keys (:read-map env)))
      (:operations env))))

(defn- validate-actions [env]
  (let [actions (into []
                      (comp
                        (drop-while (complement #{:lajt.op.stage/action}))
                        (drop 1)
                        (take-while (complement #{:lajt.op.stage/did-action})))
                      (::ops-to-run env))]
    (when-not (== 1 (count actions))
      (action-error env actions))
    env))

(def stage-operators
  {:lajt.op.stage/will-setup check-for-read-map-modification
   :lajt.op.stage/setup      check-for-read-map-modification
   :lajt.op.stage/transform  (fn [_ env]
                               (assoc env :result (ops/get-result env)))})

(def stage-validators
  {:lajt.op.stage/action
   validate-actions
   :lajt.op.stage/did-action
   ;; Add the pull op to the read-map.
   (fn [env]
     (if-not (and (some? (:query env))
                  (ops/should-pull? env)
                  (nil? (get-in env [:read-map ::ops/pull])))
       env
       (let [env (assoc-in env [:read-map ::ops/pull] (:query env))]
         (reduced
           {::new-env    env
            ;; remove all ops until did-action to continue
            ;; where we are in this process.
            ::ops-to-run (eduction
                           (drop-while
                             (complement
                               #{:lajt.op.stage/did-action}))
                           (drop 1)
                           (ops-to-run env))}))))})

(defn- perform-operation [env op]
  ;; It's a stage keyword.
  (if (= "lajt.op.stage" (and (keyword? op) (namespace op)))
    (let [validator (get stage-validators op identity)]
      (validator (assoc env ::execution-stage op)))
    ;; It's an operator-stage pair.
    (let [[op-id stage-id] op
          stage ((::get-stage env) op-id stage-id)
          _ (when-not (:lajt.op.stage/fn stage)
              (prn "NO FN: " stage)
              (prn "op: " op))
          should-call? (contains? (:read-map env) op-id)
          ret (if should-call?
                (do (prn "performing op: " op)
                    ((:lajt.op.stage/fn stage)
                      env
                      (get-in env [:read-map op-id])))
                env)
          stage-op (get stage-operators
                        (::execution-stage env)
                        #(-> %2))
          ret (stage-op env ret)]
      (when should-call?
        #?(:clj
           (prn "performed " op
                "diff: " (butlast (clojure.data/diff env ret)))))
      ret)))

(defn perform-read2 [env]
  (prn "read-key: " (:read-key env) "ops-to-run: " (::ops-to-run env))
  (let [ret (reduce perform-operation env (::ops-to-run env))]
    (if-let [new-env (::new-env ret)]
      (recur (assoc new-env ::ops-to-run
                            (or (::ops-to-run ret)
                                (and (= (:read-map new-env)
                                        (:read-map env))
                                     (= (:operations new-env)
                                        (:operations env))
                                     (::ops-to-run new-env))
                                (ops-to-run new-env))))
      ret)))

(comment
  (let [operations @ops/operations
        operations-by-op-id (into {}
                                  (map (juxt :lajt.op/id identity))
                                  operations)
        env {:read-map {:query {}
                        :sort {}
                        :params {}
                        :case {}}}
        ops (vals
              (select-keys operations-by-op-id
                           (keys (:read-map env))))]
    (ops/operation-order ops)
    (eduction
      (filter (comp (set (keys (:read-map env)))
                    :lajt.op/id))
      ops))
  (eduction
    (filter (let [s (set (keys (:read-map env)))]
              (comp s :lajt.op/id)))
    OPS)

  (def ENV {:read-map {:query {}
                       :sort {}
                       :params {}
                       :case {}}})

  (ops/operation-order
    (eduction
      (filter (comp (set (keys (:read-map ENV)))
                    :lajt.op/id))
      OPS))
  (def ops-to-run *1)
  (->> ops-to-run
       (drop-while (complement #{:lajt.op.stage/action}))
       (drop 1)
       (take-while (complement #{:lajt.op.stage/did-action})))

  (def stages ops/execution-stages)
  (eduction
    (mapcat (fn [{:lajt.op/keys [id stages]}]
              (eduction (map #(assoc % :lajt.op/id id)) stages)))
    (filter #(contains? stages (:lajt.op.stage/id %)))
    OPS)
  )

(comment
  ;; This might be useful for some type of integration between om.next and our stuff?
  (defn- wrap-query-in-join-ast [env remote-ret]
    (if-some [->ast (:om.next.parser.impl/expr->ast env)]
      (let [{:keys [join-namespace]} ((:parser env))
            join-key (keyword (name join-namespace) (name (gensym)))]
        (->ast {join-key remote-ret}))
      (throw (ex-info (str "Must pass :om.next/expr->ast to env when "
                           "using lajt.read/om-next-value-wrapper.")
                      {})))))

(defn om-next-value-wrapper
  "To use when lajt reads are used together with the om.next parser."
  [read]
  (fn [env k p]
    (let [ret (read env k p)
          ret (if (fn? ret) (ret env) ret)]
      (when (some? ret)
        (if-some [t (:target env)]
          {t ret}
          {:value ret})))))

(defn perform-remote [{:keys [target]
                       ;; Using some internals
                       :lajt.parser/keys [expr expr-spec merge-queries]
                       :as env}]
  (let [env (perform-pre-ops env)
        read-map (:read-map env)
        ret (get read-map target)
        ;; Value of :remote can be a function or a vector of functions:
        ret (cond-> ret
                    (not (or (boolean? ret) (nil? ret)))
                    (ops/call-fns env))
        ;; Wrap it in a sequential collection if it isn't already.
        ;; We're able to return multiple remote queries.
        ret (cond-> ret
                    (not (sequential? ret))
                    vector)
        ;; If the value is true, unform the expression to its original form.
        ;; Remove falsey values.
        ret (into []
                  (comp (filter boolean)
                        (map #(if (true? %) (s/unform expr-spec expr) %)))
                  ret)
        ;; If we've got any additional pull patterns from any of the ops,
        ;; add them now.
        ret (merge-queries ret
                           ;; TODO: Remove this hack.
                           ;; Need some remote data pipeline.
                           ;; Need to remove the hard coded ordering of pre-post ops.
                           ;; Stuff!
                           ;; TODO: Using multiple defmulti's seem like a good idea?
                           ;; Or is protocols better?
                           ;; defop (reify IPre IPost IAction IRemoteData ILocalData).
                           (when (contains? read-map :sort)
                             (let [env' (ops/call env :sort (get read-map :sort))
                                   q (:remote-query (ops/get-scoped env' ::ops/sort))]
                               q)))
        ;; Get the dependencies from the results of :depends-on
        deps (::ops/results env)]
    (into (vec deps) ret)))

(defn ->read-fn [lajt-reads db-fns]
  (fn [env k p]
    (binding [ops/*debug* (:debug env false)]
      (when (and ops/*debug* (nil? (:target env)))
        (locking *out*
          (prn "Calling read: " k)))
      (let [operations (:operations env @ops/operations)
            read-map (lajt-reads k)
            env (assoc env :read-params p
                           :read-key k
                           :reads lajt-reads
                           :read-map read-map
                           :db-fns db-fns
                           :read-ops (:read-ops env default-ops)
                           :operations operations
                           ::get-stage (ops/get-stage-fn operations))]
        (validate-read! env)
        (if (some? (:target env))
          (perform-remote env)
          (ops/get-result
            (perform-read2
              (assoc env ::ops-to-run (ops-to-run env)))))))))
