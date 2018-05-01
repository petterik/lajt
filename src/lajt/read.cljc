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

;; TODO: YOU ARE HERE !
;; TODO: assoc :result after each transform.
(def stage-operators {:lajt.op.stage/transform identity})

(defn perform-read2 [env]
  (let [operations @ops/operations
        operations-by-op-id (into {}
                                  (map (juxt :lajt.op/id identity))
                                  operations)]
    ;; TODO: Check if the :read-map has changed and restart the thing.
    (reduce (fn [env op]
              (if (keyword? op)
                (assoc env ::execution-stage op)
                (let [[op-id stage] op
                      ret ((:lajt.op.stage/fn stage)
                            env
                            (get-in env [:read-map op-id]))
                      stage-op (get stage-operators
                                    (::execution-stage env)
                                    identity)]
                  (stage-op ret))))
            env
            (ops/operation-order
              (vals
                (select-keys operations-by-op-id
                             (keys (:read-map env))))))))

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
      (let [env (assoc env :read-params p
                           :read-key k
                           :reads lajt-reads
                           :read-map (lajt-reads k)
                           :db-fns db-fns
                           :read-ops (:read-ops env default-ops))]
        (validate-read! env)
        (if (some? (:target env))
          (perform-remote env)
          (ops/get-result (perform-read env)))))))
