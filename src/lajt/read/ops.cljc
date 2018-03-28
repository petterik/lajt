(ns lajt.read.ops
  (:require
    [medley.core :as m]
    [clojure.spec.alpha :as s]
    [clojure.string :as string]
    [clojure.spec.gen.alpha :as gen]))

(defn assoc-scoped [env k v]
  (assoc-in env [k (:read-key env)] v))

(defn get-scoped [env k]
  (get-in env [k (:read-key env)]))

(defn add-result
  "Adds a result to env. Convenicence function for actions."
  [env res]
  (assoc-scoped env :result res))

(defn get-result
  [env]
  (get-scoped env :result))

;; ############
;; Ops

(defmulti call-op (fn [env k v] k) :default ::default)

(defmethod call-op ::default
  [env k v]
  (throw (ex-info (str "No read-op for key: " k)
                  {:key k :value v :query (:query env)})))

(defmethod call-op :depends-on
  [env _ v]
  (let [res ((:parser env) env v)
        env (update env :results merge res)]
    ;; Assoc the :depends-on key with all results
    ;; such reads can access it easily.
    (assoc env :depends-on (:results env))))

(defn call-fns [x env]
  (cond (fn? x) (x env)
        (vector? x) (reduce #(%2 %1) env x)
        :else
        (throw (ex-info "Unknown call chain: " x
                        {:call-chain x
                         :env        env}))))

(defmethod call-op :before
  [env _ v]
  (call-fns v env)
  env)

(defmethod call-op :params
  [env _ params]
  (assoc-scoped env :params
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
  (let [q-params (get-scoped env :params)
        query (update query :in #(into (vec (or % '[$]))
                                       (keys q-params)))
        res (apply (get-in env [:db-fns :q]) query (:db env)
                   (vals q-params))]
    #_(prn {:op     k
            :result res
            :query  query
            :params q-params})
    (add-result env res)))

(defmethod call-op :lookup-ref
  [env _ ref]
  (add-result env ((get-in env [:db-fns :entid]) (:db env) ref)))

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

(s/def ::find-pattern (s/or :relation (s/+ ::logic-var)
                            :tuple (s/tuple (s/+ ::logic-var))
                            :scalar (s/cat :symbol ::logic-var
                                           :dot '#{.})
                            :collection (s/tuple (s/cat :symbol ::logic-var
                                                        :dots '#{...}))))

(defn find-pattern-type [env read-map]
  (if (some? (:lookup-ref read-map))
    :scalar
    (let [[type] (s/conform ::find-pattern
                            (get-in read-map [:query :find]))]
      type)))

(def find-type->pull-fn
  {:scalar     :pull
   :collection :pull-many})

(defmethod call-op ::pull
  [{:keys [read-map] :as env} _ query]
  (let [find-type (find-pattern-type env read-map)
        result (get-result env)
        pull-fn (get-in env [:db-fns (find-type->pull-fn find-type)])]
    (when (nil? pull-fn)
      (throw (ex-info (str "WARN: Tried to perform a pull on a query that"
                           " was not of :scalar or :collection find-pattern type.")
                      {:read-map  read-map
                       :query     query
                       :find-type find-type})))
    (add-result env (pull-fn (:db env) query result))))
