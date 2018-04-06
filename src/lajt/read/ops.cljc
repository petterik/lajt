(ns lajt.read.ops
  (:require
    #?(:clj
        [clojure.data])
    [medley.core :as m]
    [clojure.spec.alpha :as s]
    [clojure.string :as string]
    [clojure.spec.gen.alpha :as gen]))

(def ^:dynamic *debug*)

(defn assoc-scoped [env k v]
  (assoc-in env [k (:read-key env)] v))

(defn get-scoped [env k]
  (get-in env [k (:read-key env)]))

(defn add-result
  "Adds a result to env. Convenicence function for actions."
  [env res]
  (assoc-scoped env :results res))

(defn get-result
  [env]
  (get-scoped env :results))

(defn remove-pull [env]
  (update env :read-map dissoc ::pull))

;; ############
;; Ops

(defmulti call-op (fn [env k v] k) :default ::default)

(defmethod call-op ::default
  [env k v]
  (throw (ex-info (str "No read-op for key: " k)
                  {:key k :value v :query (:query env)})))

(defmethod call-op :depends-on
  [env _ v]
  (let [query (if (fn? v) (v env) v)
        res ((:parser env) env query)
        env (if (some? (:target env))
              (update env :results (fnil into []) res)
              (update env :results merge res))]
    ;; Assoc the :depends-on key with all results
    ;; such reads can access it easily.
    (assoc env :depends-on (:results env))))

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

(defmethod call-op :before
  [env _ v]
  (call-fns v env)
  env)

(defmethod call-op :params
  [env _ params]
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
                     {:params params}))))))

(defmethod call-op :query
  [env k query]
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
        (add-result env res)))))

(defmethod call-op :lookup-ref
  [env _ ref]
  (add-result env ((get-in env [:db-fns :entid]) (:db env) ref)))

(defmethod call-op :no-op
  [env _ _]
  (remove-pull env))

(defmethod call-op :sort
  [env _ sort-map]
  (let [result (get-result env)
        ret (if (or (map? result) (not (coll? result)))
              [result]
              (let [{:keys [comparator key-fn order entities?]
                     :or   {entities?  true
                            comparator compare}} sort-map
                    comparator (if (= :decending order)
                                 (fn [a b]
                                   (comparator b a))
                                 comparator)
                    key-fn (if (and (some? key-fn) entities?)
                             (let [{:keys [entity]} (:db-fns env)
                                   db (:db env)
                                   entity* (memoize #(entity db %))]
                               (fn [a]
                                 (key-fn (entity* a))))
                             key-fn)]
                (if (some? key-fn)
                  (sort-by key-fn comparator result)
                  (sort comparator result))))]
    (add-result env ret)))

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
(s/conform ::find-pattern '[(count ?e) .])

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

(defmethod call-op ::pull
  [{:keys [read-map] :as env} _ query]
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
                             (pull-fn (:db env) query)))))

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

(defmethod call-op :case
  [{:keys [read-map target] :as env} _ cases]
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
    (assoc env :read-map read-map)))

(defmethod call-op :after
  [env _ calls]
  ;; :after sets the result to whatever the functions return.
  ;; Functions are passed the result as argument.
  (add-result env (call-fns calls env)))

(defmethod call-op :custom
  [env _ custom-fn]
  ;; :after sets the result to whatever the functions return.
  ;; Functions are passed the result as argument.
  (add-result env (custom-fn env)))

(defn call [env k v]
  #?(:cljs
     (call-op env k v)
     :clj
     (try
       (when *debug*
         (locking *out*
           (prn "Entering op: " k " for k: " (:read-key env))))
       (let [ret (call-op env k v)]
         (when *debug*
           (let [[before after] (clojure.data/diff env ret)]
             (locking *out*
               (prn {:op     k
                     :before before
                     :after  after}))))
         ret)
       (catch Throwable t
         (when *debug*
           (locking *out*
             (prn {:op     k
                   :before (select-keys env [:read-map :read-key :query])
                   :after  :exception
                   :ex     (Throwable->map t)})))
         (throw t)))))

(comment
  (let [ret nil]
    (when-not (map? ret)
      (throw (ex-info (str "Call to op: " k
                           " did not return a map."
                           " Must return a new environment")
                      {:op k :val v})))
    ret))