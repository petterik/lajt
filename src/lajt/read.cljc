(ns lajt.read
  (:require
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [clojure.spec.gen.alpha :as gen]
    [medley.core :as m]
    #?(:clj
    [clojure.data])))

(def ^:dynamic *debug*)

(def default-ops
  {:pre     [:depends-on :params :before]
   :actions [:query :lookup-ref]
   :post    [:sort ::pull]})

{'[?person ...] [:depends-on ::people]
 ;; Look how easy it is to get the route now.
 '?route        [:route]
 '?email        [:route-param :email]}

(comment
  :sort-map
  {
   ;; sorts by the key of this function
   :key-fn     :person/first-name
   ;; defaults to true. Calls d/entity on the values before the :key-fn is called.
   ;; Makes it easy to sort entity id's by some key on that entity.
   :entities?  true
   ;; defaults to clojure.core/compare
   :comparator (fn [a b])
   ;; if set to :decending, it'll efficiently reverse the sorting by swapping the
   ;; arguments to the sort comparator.
   :order      :decending
   })

(defn logic-var? [sym]
  (and (symbol? sym)
       (nil? (namespace sym))
       (str/starts-with? (name sym) "?")))

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
  {:scalar :pull
   :collection :pull-many})

(defn call-fns [x env]
  (cond (fn? x) (x env)
        (vector? x) (reduce #(%2 %1) env x)
        :else
        (throw (ex-info "Unknown call chain: " x
                        {:call-chain x
                         :env        env}))))

(defmulti call-read-op (fn [env k v] k) :default ::default)
(defn call-op [env k v]
  (let [env' (call-read-op env k v)]
    #?(:clj
       (when *debug*
         (let [[before after] (clojure.data/diff env env')]
           (prn {:op     k
                 :before before
                 :after  after}))))
    env'))

(defmethod call-read-op ::default
  [env k v]
  (throw (ex-info (str "No read-op for key: " k)
                  {:key k :value v :query (:query env)})))

(defmethod call-read-op :depends-on
  [env _ v]
  (let [res ((:parser env) env v)
        env (update env ::results merge res)]
    ;; Assoc the :depends-on key with all results
    ;; such reads can access it easily.
    (assoc env :depends-on (::results env))))

(defmethod call-read-op :before
  [env _ v]
  (call-fns v env)
  env)

(defn- assoc-scoped [env k v]
  (assoc-in env [k (::read-key env)] v))

(defn- get-scoped [env k]
  (get-in env [k (::read-key env)]))

(defn- add-result
  "Adds a result to env. Convenicence function for actions."
  [env res]
  (assoc-scoped env ::result res))

(defn- get-result
  [env]
  (get-scoped env ::result))

(defmethod call-read-op :params
  [env _ params]
  (assoc-scoped env ::params
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

(defmethod call-read-op :query
  [env k query]
  (let [q-params (get-scoped env ::params)
        query (-> (dissoc query :params)
                  (update :in #(into (vec (or % '[$]))
                                     (keys q-params))))
        res (apply (get-in env [::db-fns :q]) query (:db env)
                   (vals q-params))]
    #_(when *debug*
        (prn {:op     k
              :result res
              :query  query
              :params q-params}))
    (add-result env res)))

(defmethod call-read-op :lookup-ref
  [env _ ref]
  (add-result env ((get-in env [::db-fns :entid]) (:db env) ref)))

(defmethod call-read-op :sort
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
                             (let [{:keys [entity]} (::db-fns env)
                                   db (:db env)
                                   entity* (memoize #(entity db %))]
                               (fn [a]
                                 (key-fn (entity* a))))
                             key-fn)]
                (if (some? key-fn)
                  (sort-by key-fn comparator result)
                  (sort comparator result))))]
    (add-result env ret)))

(defmethod call-read-op ::pull
  [{::keys [read-map] :as env} _ query]
  (let [find-type (find-pattern-type env read-map)
        result (get-result env)
        pull-fn (get-in env [::db-fns (find-type->pull-fn find-type)])]
    (when (nil? pull-fn)
      (throw (ex-info (str "WARN: Tried to perform a pull on a query that"
                           " was not of :scalar or :collection find-pattern type.")
                      {:read-map  read-map
                       :query     query
                       :find-type find-type})))
    (add-result env (pull-fn (:db env) query result))))

(defn- validate-read! [{::keys [reads read-key] :as env}]
  (let [read-map (reads read-key)]
    (when (nil? read-map)
      (throw (ex-info (str "No such read: " read-key)
                      {:read-key read-key
                       :query    (:query env)})))))

(defn- get-action [{::keys [read-map read-key read-ops] :as env}]
  (let [actions (filterv (set (:actions read-ops)) (keys read-map))]
    (if (== 1 (count actions))
      (first actions)
      (throw
        (ex-info
          (if (zero? (count actions))
            (str "read-map did not contain any of the actions."
                 " It must contain one of: "
                 (:actions read-ops))
            (str "Cannot have multiple actions"
                 " in the read's map. Had actions: "
                 actions))
          {:actions  (:actions read-ops)
           :read-key read-key
           :read-map read-map
           :query    (:query env)})))))

(defn- perform-ops [env ops]
  (reduce (fn [{::keys [read-map] :as env} read-op]
            (cond-> env
                    (contains? read-map read-op)
                    (call-op read-op
                             (get read-map read-op))))
          env
          ops))

(defn perform-read [{::keys [reads read-key read-ops] :as env}]
  (validate-read! env)
  (let [
        ;; Adds read-map with an additional ::pull action
        ;; whenever there's a query.
        env (cond-> (assoc env ::read-map (reads read-key))
                    (not-empty (:query env))
                    (assoc-in [::read-map ::pull] (:query env)))
        ;; Performs the :pre ops
        env (perform-ops env (:pre read-ops))
        ;; Call an :action
        action (get-action env)
        env (call-op env action (get-in env [::read-map action]))
        ;; Call :post ops
        env (perform-ops env (:post read-ops))]
    env))

{:query      '{:find  [?e .]
               :where [[?e :address/email ?email]
                       [?e :address/person ?person]]}
 ;; Depends on reads in this vector (or set).
 :depends-on [::people]
 ;; Pass the depends results in to the params.
 ;; Still unsure if the params fn should take a map or
 ;; something like: [env route-params depends].
 :params     (fn [{:keys [depends route-params]}]
               ;; What to do about nil values?
               ;; We could make this declarative like:
               {'[?person ...] (::people depends)
                '?email        (:email route-params)}
               ;; But this would yield different results from:
               (cond-> {}
                       (some? (::people depends))
                       (assoc '[?person ...] (::people depends))
                       (some? (:email route-params))
                       (assoc '?email (:email route-params)))
               ;; I think it makes sense to just mean different things
               ;; depending on how one declares the params, INSTEAD OF
               ;; removing symbols mapped to nil values.
               )}

(defn om-next-value-wrapper [read]
  (fn [env k p]
    {(or (:target env) :value) (read env k p)}))

(defn ->read-fn [lajt-reads db-fns]
  (fn [env k p]
    (binding [*debug* (:debug env false)]
      (let [env (assoc env :params p
                           ::read-key k
                           ::reads lajt-reads
                           ::db-fns db-fns
                           ::read-ops (:read-ops env default-ops))]
        (if-let [remote (:target env)]
          (get (lajt-reads k) remote)
          (get-result (perform-read env)))))))


