(ns lajt.read
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.spec.gen.alpha :as gen]))

(defn- query-params [env {:keys [params] :as read-map}]
  (when (some? params)
    (cond
      (map? params)
      (reduce-kv (fn [m k v]
                   (assoc m k (reduce #(%2 %1) env v)))
                 {}
                 params)

      (fn? params)
      (params env)

      :else (throw (ex-info ":params need to be a map or a function." {:read-map read-map})))))

{'[?person ...] [:depends-on ::people]
 ;; Look how easy it is to get the route now.
 '?route        [:route]
 '?email        [:route-param :email]}

(defn- perform-query [env read-map]
  (let [{:keys [q]} (::db-fns env)
        q-params (query-params env read-map)
        query (update (:query read-map) :in #(into (vec (or % '[$])) (keys q-params)))]
    (apply q query (:db env) (vals q-params))))

(defn- sort-result [env read-map result]
  (if-not (coll? result)
    [result]
    (let [{:keys [comparator key-fn order entities?]
           :or {entities? true
                comparator compare}} (:sort read-map)

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
        (sort comparator result)))))

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
  (let [[type] (s/conform ::find-pattern (get-in read-map [:query :find]))]
    type))

(def find-type->pull-fn
  {:scalar :pull
   :collection :pull-many})

(defn perform-pull [env read-map result]
  (let [find-type (find-pattern-type env read-map)]
    (if-some [pull-fn (get-in env [::db-fns (find-type->pull-fn find-type)])]
      (pull-fn (:db env) (:query env) result)
      (throw (ex-info (str "WARN: Tried to perform a pull on a query that"
                           " was not of :scalar or :collection find-pattern type.")
                      {:read-map  read-map
                       :query     (:query env)
                       :find-type find-type})))))

(defn perform-read [{::keys [reads read-key] :as env}]
  (let [read-map (reads read-key)
        deps (:depends-on read-map)
        query (:query env)
        env (reduce (fn [env dep]
                      (if (some? (get-in env [::results dep]))
                        env
                        (perform-read (assoc env ::read-key dep))))
                    ;; TODO: let :depends-on have their own queries?
                    (dissoc env :query)
                    deps)
        env (assoc env :query query
                       :depends-on (select-keys (::results env) deps))]
    (assoc-in env
              [::results read-key]
              (cond->> (perform-query env read-map)
                       (some? (:sort read-map))
                       (sort-result env read-map)
                       (not-empty (:query env))
                       (perform-pull env read-map)))))

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
    (let [env (assoc env :params p
                         ::read-key k
                         ::reads lajt-reads
                         ::db-fns db-fns)]
      (if-let [remote (:target env)]
        (get (lajt-reads k) remote)
        (get-in (perform-read env) [::results k])))))