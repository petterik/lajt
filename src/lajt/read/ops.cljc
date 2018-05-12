(ns lajt.read.ops
  (:require
    #?(:clj
        [clojure.data])
    [medley.core :as m]
    [clojure.spec.alpha :as s]
    [clojure.set :as set]
    [clojure.string :as string]
    [clojure.spec.gen.alpha :as gen]
    [com.stuartsierra.dependency :as dep]))

(def ^:dynamic *debug*)

(def stage-execution-order
  [:lajt.op.stage/will-setup
   :lajt.op.stage/setup
   :lajt.op.stage/did-setup
   :lajt.op.stage/action
   :lajt.op.stage/did-action
   :lajt.op.stage/transform
   :lajt.op.stage/did-transform])

(def remote-stage-execution-order
  [:lajt.op.stage/will-setup
   :lajt.op.stage/setup
   :lajt.op.stage/did-setup
   :lajt.op.stage/remote
   :lajt.op.stage/did-remote])

(def execution-stages (set/union
                        (set stage-execution-order)
                        (set remote-stage-execution-order)))

(s/def :lajt.op/exeuction-stages execution-stages)

(s/def :lajt.op.stage/id execution-stages)
(s/def :lajt.op.stage/fn fn?)
(s/def :lajt.op.stage/dependents (s/coll-of :lajt.op/id))
(s/def :lajt.op.stage/depends-on (s/coll-of :lajt.op/id))
(s/def :lajt.op/stage (s/keys :req [:lajt.op.stage/id
                                    :lajt.op.stage/fn]
                              :opt [:lajt.op.stage/dependents
                                    :lajt.op.stage/depends-on]))
(s/def :lajt.op/stages (s/coll-of :lajt.op/stage))
(s/def :lajt.op/id keyword?)
(s/def ::op (s/keys :req [:lajt.op/id :lajt.op/stages]))

{:lajt.op/id    :sort
 :lajt.op/stages [{:lajt.op.stage/id      :op.stage/setup
                   :lajt.op.stage/dependents []
                   :lajt.op.stage/depends-on []
                   :lajt.op.stage/fn         (fn [env v])}
                  {:lajt.op.stage/id      :op.stage/transform
                   :lajt.op.stage/dependents []
                   :lajt.op.stage/depends-on []
                   :lajt.op.stage/fn         (fn [env v])}]}
;; This is all we need for an op right now.
;; Need to know when it should be run (type).
;; We need to know if it should depend on other ops.
;; Need to know if it has other ops depending on it (dependents).
;; We need to be able to call the op.

(s/def ::->op-args
  (s/+ (s/cat :id :lajt.op.stage/id
              :deps (s/* (s/cat :deps-key #{:lajt.op.stage/dependents
                                            :lajt.op.stage/depends-on}
                                :deps (s/or :lajt.op.stage/dependents
                                            :lajt.op.stage/depends-on)))
              :fn :lajt.op.stage/fn)))

(defn ->op [id args]
  {:lajt.op/id
   id
   :lajt.op/stages
   (into []
         (map (fn [{:keys [id deps] :as stage}]
                (reduce
                  (fn [m {:keys [deps-key deps]}]
                    (update m deps-key
                            (fnil into [])
                            (remove (set (get m deps-key)))
                            (second deps)))
                  {:lajt.op.stage/id id
                   :lajt.op.stage/fn (:fn stage)}
                  deps)))

         (s/conform ::->op-args args))})

(def operations (atom #{}))

(defn def-operation! [op-key & args]
  (let [op (->op op-key
                 (map #(if (simple-keyword? %)
                         (keyword "lajt.op.stage" (name %))
                         %)
                      args))]
    (swap! operations conj op)))

;; ############
;; Impl stuff

(defn get-scoped [env k]
  (get-in env [k (:read-key env)]))

(defn assoc-scoped [env k v]
  (assoc-in env [k (:read-key env)] v))

(defn update-scoped [env k & args]
  (apply update-in env [k (:read-key env)] args))

(defn add-remote-query [env query]
  (update-scoped env ::queries (fnil into []) query))

(defn get-remote-query [env]
  (get-scoped env ::queries))

(defn add-result
  "Adds a result to env. Convenicence function for actions."
  [env res]
  (assoc-scoped env ::results res))

(defn get-result
  [env]
  (get-scoped env ::results))

(defn remove-pull-op [env]
  (assoc env ::pull ::dont-pull))

(defn should-pull? [env]
  (not= (::pull env) ::dont-pull))

;; ############
;; Ops

(def-operation! :depends-on
  :setup
  (fn [env v]
    (let [query (if (fn? v) (v env) v)
          res ((:parser env) env query (:target env))]
      (if (some? (:target env))
        (add-remote-query env res)
        (let [env (update env ::results merge res)]
          ;; Assoc the :depends-on key with all results
          ;; such reads can access it easily.
          (assoc env :depends-on (::results env)))))))

(defn call-fns
  "Calls a single function or a vector of functions with the env."
  [x env]
  (when (some? x)
    (cond (fn? x) (x env)
          (vector? x) (reduce #(%2 %1) env x)
          :else
          (throw (ex-info (str "Unknown call chain: " x)
                          {:call-chain x
                           :env        env})))))

(def-operation! :before
  :lajt.op.stage/did-setup
  (fn [env v]
    (call-fns v env)
    env))

(def-operation! :params
  :setup
  :depends-on [:depends-on]
  (fn [env params]
    (assoc env :params
               (when (some? params)
                 (cond
                   (map? params)
                   (m/map-vals #(call-fns % env) params)

                   (fn? params)
                   (params env)
                   :else
                   (throw
                     (ex-info
                       ":params need to be a map or a function."
                       {:params params})))))))

(defn- reverse-lookup-keyword [attr]
  (keyword
    (namespace attr)
    (str "_" (name attr))))

(defn make-pattern [from-sym references]
  (let [remove-visited
        (remove (fn [[_ logic-var]]
                  (and (some? logic-var)
                       (contains? (::visited references) logic-var))))]
    (into
      []
      (comp
        remove-visited
        (mapcat (fn [[attr logic-var]]
                  (if (and (some? logic-var)
                           (->> (get references logic-var)
                                (sequence remove-visited)
                                (some seq)))
                    [{attr
                      (make-pattern logic-var
                                    (update references
                                            ::visited
                                            (fnil conj #{})
                                            from-sym))}]
                    [attr]))))
      (get references from-sym))))

(defn- query->pull-pattern [query]
  (let [conformed-find (s/conform ::find-pattern (:find query))
        find-sym (condp = (first conformed-find)
                   :scalar (-> (second conformed-find) :find-element second)
                   :collection (-> (second conformed-find)
                                   first :find-element second)
                   nil)]
    (when (some? find-sym)
      (let [references
            (transduce
              (comp
                (filter (fn [[e]] (s/valid? ::logic-var e)))
                (filter (fn [[_ a]] (s/valid? keyword? a))))
              (completing
                (fn [m [e a v]]
                  (-> (update m e (fnil conj []) [a v])
                      (cond-> (s/valid? ::logic-var v)
                              (update v (fnil conj [])
                                      [(reverse-lookup-keyword a) e])))))
              {}
              (:where query))]
        (make-pattern find-sym references)))))

(def-operation! :query
  :remote
  (fn [env query]
    (if-let [pp (not-empty (query->pull-pattern query))]
      (add-remote-query env [{(:read-key env) pp}])
      env))
  :action
  (fn [env query]
    (let [q-params (:params env)
          values-set? (every? (comp some? val) q-params)]
      (if-not values-set?
        (do
          (when *debug*
            (locking *out*
              (prn "WARN: Not all params were set when executing query: "
                   (:read-key env)
                   " Returning nil.")))
          env)
        (let [query (update query :in #(into (vec (or % '[$]))
                                             (keys q-params)))
              res (apply (get-in env [:db-fns :q]) query (:db env)
                         (vals q-params))]

          #_(prn {:op     k
                  :result res
                  :query  query
                  :params q-params})
          (add-result env res))))))

(def-operation! :lookup-ref
  :action
  (fn [env ref]
    (add-result env ((get-in env [:db-fns :entid]) (:db env) ref))))

(def-operation! :no-op
  :action
  (fn [env _]
    (remove-pull-op env)))

;; TODO: Make it expand from :sort -> :sort/pre :sort/post :sort/remote
;; are expansions different from pre?
;; why?
(def-operation! :sort
  :remote
  (fn [env {:keys [key-fn entities?]
            :or   {entities?  true}}]
    (cond-> env
            (and entities? (keyword? key-fn))
            (add-remote-query [{(:read-key env) [key-fn]}])))
  :transform
  (fn [env {:keys [comparator key-fn order entities?]
            :or   {entities?  true
                   comparator compare}}]
    (let [result (get-result env)
          comparator (if (= :decending order)
                       (fn [a b]
                         (comparator b a))
                       comparator)
          !key-fn (if (and (some? key-fn) entities?)
                    (let [{:keys [entity]} (:db-fns env)
                          db (:db env)
                          entity* (memoize #(entity db %))]
                      (fn [a]
                        (key-fn (entity* a))))
                    key-fn)
          ret (if (or (map? result) (not (coll? result)))
                [result]
                (if (some? !key-fn)
                  (sort-by !key-fn comparator result)
                  (sort comparator result)))]
      (add-result env ret))))

;; ###############
;; Pull implementation

;; TODO: Move these specs somewhere?
(defn logic-var? [sym]
  (and (symbol? sym)
       (nil? (namespace sym))
       (string/starts-with? (name sym) "?")))

(s/def ::logic-var (s/with-gen logic-var?
                               #(gen/fmap (fn [sym] (symbol (str "?" (name sym))))
                                          (s/gen symbol?))))

(s/def ::aggregate (s/cat :fn symbol? :args (s/+ ::logic-var)))

(s/def ::find-element (s/or :logic-var ::logic-var
                            :aggregate ::aggregate))

(s/def ::find-pattern (s/or :relation (s/+ ::find-element)
                            :tuple (s/tuple (s/+ ::find-element))
                            :scalar (s/cat :find-element ::find-element
                                           :dot '#{.})
                            :collection (s/tuple (s/cat :find-element
                                                        ::find-element
                                                        :dots
                                                        '#{...}))))
(s/conform ::find-pattern '[[?e ...]])

(defn- pull-fn-by-action-result [env]
  (let [res (get-result env)]
    (if (or (number? res)
            ;; Lookup ref results
            (and (sequential? res)
                 (== 2 (count res))
                 (keyword? (first res))
                 (some? (second res))))
      :pull
      (when (and (sequential? res)
                 (every? pos-int? res))
        :pull-many))))

(defn pull-type [env read-map]
  (cond
    ;; TODO: let actions define if they can pull or not?
    (some? (:lookup-ref read-map))
    (pull-fn-by-action-result env)

    (some? (:query read-map))
    (let [[type m] (s/conform ::find-pattern (get-in read-map [:query :find]))
          [find-element] (:find-element m)]
      ;; An aggregate can return any type of result.
      ;; Look for the find-pattern type in the result.
      (if (= :aggregate find-element)
        (pull-fn-by-action-result env)
        (condp = type
          :scalar :pull
          :collection :pull-many
          ;; TODO: Returning nil on :relation.
          ;; Is this what we want?
          nil)))

    (some? (:custom read-map))
    (pull-fn-by-action-result env)
    :else
    (throw (ex-info (str ":find-pattern-type not implemented for "
                         "read-map with keys: " (keys read-map))
                    ;; TODO: env->ex-data
                    {:read-map read-map}))))

(def-operation! ::pull
  :lajt.op.stage/transform
  :depends-on [:sort]
  (fn [{:keys [read-map] :as env} query]
    (let [pull-type (pull-type env read-map)
          pull-fn (get-in env [:db-fns pull-type])
          result (get-result env)]
      (when (and (some? pull-type) (nil? pull-fn))
        (throw (ex-info (str "WARN: Cannot perform a pull on a query that"
                             " was not of :scalar or :collection find-pattern type.")
                        {:read-map  read-map
                         :query     query
                         :pull-type pull-type})))
      (add-result env (cond->> result
                               (and (some? result) (some? pull-fn))
                               (pull-fn (:db env) query))))))

;; base+case

(s/def ::fn-call (s/or :keyword keyword? :fn fn?))
(s/def ::fn-calls (s/coll-of ::fn-call :type vector? :gen-max 3))

(s/def ::case-pred (s/or :fn fn?
                         :case ::fn-calls))
(s/def ::case-preds (s/or
                      :one ::case-pred
                      :multiple (s/coll-of ::case-pred :type vector? :gen-max 3)))

;; TODO: The map here is really a query. Enter the query spec
;;       once we have one.
(s/def ::case-val map?)
(s/def ::case-map (s/map-of ::case-preds ::case-val
                            :conform-keys true
                            :count 1))
(s/def ::cases (s/+ ::case-map))

(comment

  (def -cases
    [{[[:route-params :petter] #(-> %)]
      {:query '{:where [[?e :person/first-name "Petter"]]}}}

     {[:route-params :diana]
      {:query '{:where [[?e :person/first-name "Diana"]]}}}

     {[[:route-params :diana]]
      {:query '{:where [[?e :person/first-name "Diana"]]}}
      }



     {#(-> % :foo) {}}])

  (s/conform ::cases -cases)
  (s/explain ::cases -cases)
  (s/conform ::case-map (first -cases))
  (s/conform ::case-pred (key (ffirst -cases)))
  (s/unform ::case-pred (s/conform ::case-pred (key (ffirst -cases))))
  )

(defn- find-case-match [env cases]
  (->> (s/conform ::cases cases)
       (some (fn [case-map]
               (let [[case-preds case-val] (first case-map)
                     preds (cond-> (second case-preds)
                                   (= :one (first case-preds))
                                   vector)
                     all? (->> preds
                               (map (fn [case-pred]
                                      (let [[type x] case-pred]
                                        (condp = type
                                          :fn (x env)
                                          :case (call-fns
                                                  (s/unform ::fn-calls x)
                                                  env)))))
                               (every? boolean))]

                 (when all?
                   (s/unform ::case-val case-val)))))))

(defmulti case-merge (fn [k o1 o2] k) :default ::default)
(defmethod case-merge ::default
  [_ o1 o2]
  (merge o1 o2))

(defn- merge-query [a b]
  ;; TODO: handle cases where a symbol is present in multiple queries.
  (-> (merge-with into
                  (dissoc a :find)
                  (dissoc b :find))
      (assoc :find (or (:find a) (:find b)))))

(defmethod case-merge :query
  [_ o1 o2]
  (merge-query o1 o2))

(def-operation! :case
  :lajt.op.stage/will-setup
  :lajt.op.stage/dependents [:lajt.op.stage/setup]
  (fn [{:keys [read-map target] :as env} cases]
    (let [base (:base read-map)
          match (find-case-match env cases)
          read-map (if (nil? match)
                     (cond-> {:no-op nil}
                             (contains? base target)
                             (assoc target (get base target)))
                     (reduce-kv (fn [m k right]
                                  (if-some [[_ left] (find m k)]
                                    (assoc m k (case-merge k left right))
                                    (assoc m k right)))
                                base
                                match))]
      ;; Changes the read-map to be the new merged map.
      (assoc env :read-map read-map))))

(def-operation! :lastly
  :lajt.op.stage/did-transform
  (fn [env calls]
    (add-result env (call-fns calls env))))

(def-operation! :custom
  :action
  (fn [env custom-fn]
    (add-result env (custom-fn env))))

;; ####################
;; ## Operation order

(defn create-stage-context [execution-order]
  (let [stage-after (into {} (map vec) (partition 2 1 execution-order))]
    {:execution-order execution-order
     :stages          (set execution-order)
     :stage-after     stage-after
     :stage-before    (into {} (map (comp vec reverse)) stage-after)}))

(def stage-contexts
  {:local (create-stage-context stage-execution-order)
   :remote (create-stage-context remote-stage-execution-order)})

(defn select-operations [filter-fn ops]
  (eduction
    (filter (comp filter-fn :lajt.op/id))
    ops))

(defn flatten-stages [stages ops]
  (eduction
    (mapcat (fn [{:lajt.op/keys [id stages]}]
              (eduction (map #(assoc % :lajt.op/id id)) stages)))
    (filter #(stages (:lajt.op.stage/id %)))
    ops))

(defn get-stage-fn [ops]
  (let [by-ids (->> (flatten-stages (constantly true) ops)
                    (group-by (juxt :lajt.op/id :lajt.op.stage/id)))]
    (fn [op-id stage-id]
      (when-let [stages (get by-ids [op-id stage-id])]
        (assert (== 1 (count stages)))
        (first stages)))))

(defn- dependency-graph [{:keys [execution-order stages stage-after]} ops]
  (let [g (reduce (fn [g [first then]]
                    (dep/depend g then first))
                  (dep/graph)
                  (->> execution-order
                       (filter (set stages))
                       (partition 2 1)))]
    (reduce
      (fn [g {:lajt.op.stage/keys [depends-on dependents] :as m}]
        (let [stage-id (:lajt.op.stage/id m)
              op-id (:lajt.op/id m)
              dep-key [op-id stage-id]
              after (stage-after stage-id)
              g (reduce #(dep/depend % dep-key [%2 stage-id]) g depends-on)
              g (reduce #(dep/depend % [%2 stage-id] dep-key) g dependents)]
          (-> g
              (dep/depend dep-key stage-id)
              (cond-> (some? after)
                      (dep/depend after dep-key)))))
      g
      (flatten-stages stages ops))))

(defn operation-order [stage-context ops]
  (dep/topo-sort (dependency-graph stage-context ops)))
