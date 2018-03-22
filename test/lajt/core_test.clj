(ns lajt.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as st]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [lajt.core :refer :all]
            [lajt.parser :as parser]
            [datascript.core :as d]
            [datascript.db]
            [clojure.string :as str]))

(defn logic-var? [sym]
  (and (symbol? sym)
       (nil? (namespace sym))
       (str/starts-with? (name sym) "?")))

(s/def :where.clause/logic-var (s/with-gen logic-var?
                                           #(gen/fmap (fn [sym]
                                                        (symbol (str "?" (name sym))))
                                                      (s/gen symbol?))))
(s/def :where.clause/ignore #{'_})
(s/def :where.clause/e (s/or :logic-var :where.clause/logic-var
                             :ignore :where.clause/ignore
                             :known nat-int?))
(s/def :where.clause/a (s/or :known keyword?))
(s/def :where.clause/v (s/or
                         :logic-var :where.clause/logic-var
                         :ignore :where.clause/ignore
                         :known some?))

(s/def :query/where-clause (s/& (s/cat :where.clause/e :where.clause/e
                                       :where.clause/a (s/? :where.clause/a)
                                       :where.clause/v (s/? :where.clause/v))
                                #(let [c (count %)]
                                   (if-some [v (:where.clause/v %)]
                                     (contains? % :where.clause/a)
                                     true))))
(s/def :query/where-clauses (s/coll-of :query/where-clause))

(s/def :query/find-pattern (s/or :relation (s/+ :where.clause/logic-var)
                                 :tuple (s/tuple (s/+ :where.clause/logic-var))
                                 :scalar (s/cat :symbol :where.clause/logic-var
                                                :dot '#{.})
                                 :collection (s/tuple (s/cat :symbol :where.clause/logic-var
                                                             :dots '#{...}))))

(s/def :query/where :query/where-clauses)
(s/def :query/find :query/find-pattern)

(s/def ::query (s/keys :req-un [:query/find :query/where]))

(def schema {:query/id            {:db/unique :db.unique/value}
             :query/where-clauses {:db/valueType   :db.type/ref
                                   :db/cardinality :db.cardinality/many}
             :query/find-pattern  {:db/valueType :db.type/ref}
             :where.clause/parts  {:db/valueType   :db.type/ref
                                   :db/cardinality :db.cardinality/many}
             :where.clause/type   {:db/index true}
             ;:where.clause/value {:db/index true}
             :where.clause/symbol {:db/index true}
             :where.clause/eav    {:db/index true}
             :where.clause/idx    {:db/index true}
             :find.pattern/type   {:db/index true}
             ;; For temporary data, to use together with d/db-with
             :temp/changed-attr   {:db/cardinality :db.cardinality/many
                                   :db/index       true}})


(defn- index-where-clauses [where-clauses]
  (->> (s/conform :query/where-clauses where-clauses)
       (into []
             (comp (map-indexed
                     (fn [idx m]
                       (map (fn [[k [t v :as value]]]
                              (merge {:where.clause/index idx
                                      :where.clause/type  (if t t :missing)
                                      :where.clause/eav   (keyword (name k))}
                                     (cond
                                       (= :logic-var t)
                                       {:where.clause/symbol v
                                        :where.clause/value  v}
                                       (= :known t)
                                       {:where.clause/value v}
                                       :else
                                       {:where.clause/value '_})))
                            (merge #:where.clause{:e nil :a nil :v nil}
                                   m))))
                   cat))))

(s/conform :query/find-pattern '[?e ?d])

(defn- index-find-pattern [find-pattern]
  (if-not (s/valid? :query/find-pattern find-pattern)
    (throw (ex-info "Spec error" (s/explain :query/find-pattern find-pattern)))
    (let [[find-type conformed] (s/conform :query/find-pattern find-pattern)]
      {:find.pattern/type    find-type
       :find.pattern/symbols (condp = find-type
                               :scalar [(:symbol conformed)]
                               :relation conformed
                               :tuple (first conformed)
                               :collection [(-> conformed first :symbol)])})))

(s/conform :query/find-pattern '[?A .])
(index-find-pattern '[?A .])

(defn index-query
  [id query]
  {:query/id            id
   :query/map           query
   :query/find-pattern  (index-find-pattern (:find query))
   :query/where-clauses (index-where-clauses (:where query))})

(def query
  {:find  '[?e .]
   :where '[[?e :foo/bar ?bar]
            [?bar :bar/baz ?baz]
            [?bar :bar/xyz _]
            [?bar :bar/missing]
            [_ :foo/bar ?bar]]})

(s/conform :query/where-clauses (:where query))

(def expected-indexed-query
  {:query/id            ::test-id
   :query/map           {:find  '[?e .]
                         :where '[[?e :foo/bar ?bar]
                                  [?bar :bar/baz ?baz]
                                  [?bar :bar/xyz _]
                                  [?bar :bar/missing]
                                  [_ :foo/bar ?bar]]}
   ;; We may want to index/parse :query/find more than just getting the raw pattern.
   :query/find-pattern  {:find.pattern/type    :scalar
                         :find.pattern/symbols ['?e]}
   :query/where-clauses '[#:where.clause{:index 0 :eav :e, :type :logic-var, :symbol ?e :value ?e}
                          #:where.clause{:index 0 :eav :a, :type :known, :value :foo/bar}
                          #:where.clause{:index 0 :eav :v, :type :logic-var, :symbol ?bar :value ?bar}
                          #:where.clause{:index 1 :eav :e, :type :logic-var, :symbol ?bar :value ?bar}
                          #:where.clause{:index 1 :eav :a, :type :known, :value :bar/baz}
                          #:where.clause{:index 1 :eav :v, :type :logic-var, :symbol ?baz :value ?baz}
                          #:where.clause{:index 2 :eav :e, :type :logic-var, :symbol ?bar :value ?bar}
                          #:where.clause{:index 2 :eav :a, :type :known, :value :bar/xyz}
                          #:where.clause{:index 2 :eav :v, :type :ignore  :value _}
                          #:where.clause{:index 3 :eav :e, :type :logic-var, :symbol ?bar :value ?bar}
                          #:where.clause{:index 3 :eav :a, :type :known, :value :bar/missing}
                          #:where.clause{:index 3 :eav :v, :type :missing :value _}
                          #:where.clause{:index 4 :eav :e, :type :ignore  :value _}
                          #:where.clause{:index 4 :eav :a, :type :known, :value :foo/bar}
                          #:where.clause{:index 4 :eav :v, :type :logic-var, :symbol ?bar :value ?bar}]})

(deftest query-indexing-test
  (is (= (index-find-pattern '[?e .])
         {:find.pattern/symbols '[?e]
          :find.pattern/type    :scalar}))
  (is (= (index-find-pattern '[?e ?b])
         {:find.pattern/symbols '[?e ?b]
          :find.pattern/type    :relation}))
  (is (= (index-find-pattern '[[?e ?b]])
         {:find.pattern/symbols '[?e ?b]
             :find.pattern/type    :tuple}))
  (is (= (index-find-pattern '[[?e ...]])
         {:find.pattern/symbols ['?e]
          :find.pattern/type    :collection}))
  (is (= (index-query ::test-id query) expected-indexed-query))

  (let [conn (d/create-conn schema)]
    (d/transact! conn [(index-query ::test-id query)])
    (is (= (d/pull (d/db conn)
                   [:query/id
                    :query/map
                    {:query/find-pattern [:find.pattern/type
                                          :find.pattern/symbols]}
                    {:query/where-clauses [:where.clause/eav
                                           :where.clause/type
                                           :where.clause/index
                                           :where.clause/value
                                           :where.clause/symbol]}]
                   [:query/id ::test-id])
           expected-indexed-query))))

;; I might have started too far down.
;; I haven't defined the goal with what I'm doing.
;; It's interesting that one can index queries in datascript.
;; I think this could be useful to create transducers for each query.
;; So from here, I want to be able to create transducers I think.
;; I also want to be able to specify values for every logic-var.
;; ^^^ this is used for when there have been updates to attributes.
;; Can I do something with the schema?
;; What if I pass the schema to index queries, so we can get the joins?
;; Interesting stuff.

(defn query-xf [query-db query-id]
  (fn [db]
    (map identity)))

(deftest indexed-query-querying-test
  (let [data-conn   (d/create-conn {:user/name      {:db/unique :db.unique/identity}
                                    :user/posts     {:db/valueType   :db.type/ref
                                                     :db/cardinality :db.cardinality/many}
                                    :post/comments  {:db/valueType   :db.type/ref
                                                     :db/cardinality :db.cardinality/many}
                                    :comment/author {:db/valueType :db.type/ref}})
        query-conn (d/create-conn schema)
        queries [{:find  '[[?e ...]]
                  :where '[[?e :user/posts ?post]
                           [?post :post/title "Post 1"]]}]]
    (d/transact! data-conn [{:user/name "Other"}
                            {:user/name  "Petter"
                             :user/posts [{:post/title    "Post 1"
                                           :post/comments [{:comment/content "Bla bla"
                                                            :comment/author  [:user/name "Other"]}
                                                           {:comment/content "Blu blu"
                                                            :comment/author  [:user/name "Petter"]}]}]}])
    (d/transact! query-conn (map-indexed index-query queries))

    #_(is (= (d/entid (d/db data-conn) [:user/name "Petter"])
           ((query-xf (d/db query-conn) 0)
                     (d/db data-conn))))))

(comment
  (let [data-conn   (d/create-conn {:user/name      {:db/unique :db.unique/identity}
                                    :user/posts     {:db/valueType   :db.type/ref
                                                     :db/cardinality :db.cardinality/many}
                                    :post/comments  {:db/valueType   :db.type/ref
                                                     :db/cardinality :db.cardinality/many}
                                    :post/title     {:db/index true}
                                    :comment/author {:db/valueType :db.type/ref}})
        query-conn (d/create-conn schema)
        queries [{:find  '[[?e ...]]
                  :where '[[?e :user/posts ?post]
                           [?post :post/title "Post 1"]]}
                 ;; Users who have commented on their own posts
                 {:find  '[[?e ...]]
                  :where '[[?e :user/posts ?post]
                           [?post :post/title "Post 1"]
                           [?post :post/comments ?comments]
                           [?comments :comment/author _]]}]]
    (d/transact! data-conn [{:user/name "Other"}
                            {:user/name  "Petter"
                             :user/posts [{:post/title    "Post 1"
                                           :post/comments [{:comment/content "Bla bla"
                                                            :comment/author  [:user/name "Other"]}
                                                           {:comment/content "Blu blu"
                                                            :comment/author  [:user/name "Petter"]}]}]}])
    (d/transact! query-conn (map-indexed index-query queries))

    (is (= (d/entid (d/db data-conn) [:user/name "Petter"])
           (first ((query-xf (d/db query-conn) 0) (d/db data-conn)))))
    [data-conn query-conn queries])

  (do (def setup *1)
      (def data-conn (first setup))
      (def query-conn (second setup))
      (def queries (nth setup 2)))

  (d/q {:find  '[[?out-symbols ?find-type]]
        :where '[[?query :query/id ?query-id]
                 [?query :query/find-pattern ?find]
                 [?find :find.pattern/type ?find-type]
                 [?find :find.pattern/symbols ?out-symbols]]
        :in    '[$ ?query-id]}
       (d/db query-conn)
       0)
  (def out-symbols (first *1))
  (def find-type (second *2))

  (d/q {:in    '[$ ?query-id [?out-symbol ...]]
        :where '[[?query :query/id ?query-id]
                 [?query :query/where-clauses ?where]
                 [?where :where.clause/symbol ?out-symbol]
                 [?where :where.clause/eav ?eav]
                 [?where :where.clause/index ?where-index]
                 [?where2 :where.clause/index ?where-index]
                 [?query :query/where-clauses ?where2]
                 (not= ?where ?where2)
                 [?where2 :where.clause/eav ?eav2]
                 (not= ?eav2 :a)]
        :find  '[?out-symbol ?eav]})

  ;; TODO: more.
  ;; Thought process:
  ;; Given a query-id and an [?out-symbol ...]
  ;; - Output filters and additional paths to new out-symbols (paths via references).
  ;;   - Filters when the :where.clause/type is :known
  ;;   - Output new symbols to follow when the other (:e or :v) is :logic-var.
  ;; Then we'll create a transducer given the output of this ^^^^ function.

  ;; To be continued...

  ;; we could return (fn [db] (partial d/datoms db <index> <components ...>))
  ;; Taking the extra component at the end
  ;; Then passing no more arguments to it, we'd be able to get the length of the datoms
  ;; So we can do the smallest set of datoms first.
  ;; Wait, we can actually calculate the shortest path.
  ;; No.

  ;; Filters should be done in the datoms call
  ;; Multiple filters can prioritize values that are indexed + their lengths.
  ;; How much does estimating length "cost"?
  ;; Often not too much? Dunno. datascript.btset/est-count

  ;; Also, the original idea probably doesn't work as well when filters are involved.

  ;; This query gets all [e a v <:where.clause/type>] from the queries.
  (d/q {:where '[[?query :query/id ?query-id]
                 [?query :query/where-clauses ?where]
                 ;; Exchange this :eav to be either :e OR :v (and make where3 be the other).
                 [?where :where.clause/eav :e]
                 [?where :where.clause/symbol ?sym]
                 [?where :where.clause/index ?index]
                 [?where :where.clause/type ?type]
                 [?where2 :where.clause/index ?index]
                 [?query :query/where-clauses ?where2]
                 [?where2 :where.clause/eav :a]
                 [?where2 :where.clause/value ?attr]
                 [?where3 :where.clause/index ?index]
                 [?query :query/where-clauses ?where3]
                 [?where3 :where.clause/eav :v]
                 [?where3 :where.clause/value ?val]
                 [?where3 :where.clause/type ?type3]]
        :find  '[?sym ?attr ?val ?type ?type3]
        :in    '[$ ?query-id]}
       (d/db query-conn)
       1)
  (comment
    (def eavtype *1)
    ;; We can use this data to group all constraints together.
    (group-by first eavtype)
    ;; =>
    '{?comments [[?comments :comment/author ?e :logic-var :logic-var]
                 [?comments :comment/content ?content :logic-var :logic-var]],
      ?post     [[?post :post/comments ?comments :logic-var :logic-var]
                 [?post :post/title "Post 1" :logic-var :known]],
      ?e        [[?e :user/posts ?post :logic-var :logic-var]]}
    ;; =>
    '{?comments [?comments ?e ?content],
      ?post     [?post ?comments],
      ?e        [?e, ?post]}

    [?comments :comment/author ?e :user/posts ?post]
    {?comments {:comment/author ?e}}

    )

  ;; Depending on how we want to find things, we'll need different strategies.
  ;; (protocol?)
  ;; for [?e .] or [[?e ...]]:

  ;; add first or into.
  (fn [db]
    (eduction
      (map :e)
      (mapcat #(d/datoms db :avet :user/posts %))
      (eduction
        (map :e)
        (filter (fn [post]
                  (seq (eduction (map :v)
                                 (mapcat #(d/datoms db :eavt % :comment/author))
                                 (d/datoms db :eavt post :post/comments)))))
        (d/datoms db :avet :post/title "Post 1"))))

  (fn [db]
    (letfn [(comments
              ([] (d/datoms db :aevt :comment/author))
              ([comments]
               (eduction
                 (mapcat #(d/datoms db :eavt % :comment/author))
                 comments))
              ([comments authors]
               (eduction
                 (mapcat #(d/datoms db :eavt % :comment/author))
                 (filter (comp (into #{} authors) :v))
                 comments)))
            (posts-fn
              ([] (eduction
                    (map :e)
                    (mapcat #(d/datoms db :eavt % :post/comments))
                    (d/datoms db :avet :post/title "Post 1")))
              ([posts comments]
               (cond
                 (and (some? posts) (some? comments))
                 (eduction
                   (mapcat #(d/datoms db :eavt % :post/title "Post 1"))
                   (map :e)
                   (mapcat #(d/datoms db :eavt % :post/comments))
                   (filter (comp (into #{} comments) :v))
                   posts)
                 (and (some? posts) (nil? comments))
                 (eduction
                   (mapcat #(d/datoms db :eavt % :post/title "Post 1"))
                   (map :e)
                   (mapcat #(d/datoms db :eavt % :post/comments))
                   posts)
                 (and (nil? posts) (some? comments))
                 (eduction
                   (mapcat #(d/datoms db :avet :post/comments %))
                   (map :e)
                   (mapcat #(d/datoms db :eavt % :post/title "Post 1"))
                   comments)
                 :else (recur (posts-fn) nil))))
            (users
              ([] (d/datoms db :aevt :user/posts))
              ([users]
                (eduction)))
            ]
         ))
  )

(defn assert-indexed [db a ex-map]
  (if-not (datascript.db/indexing? db a)
    (throw (ex-info "Getting value by :avet and attr isn't indexed."
                    ex-map))))

(defn roo [eavts]
  (fn [db context]
    (->> eavts
         (map (fn [[e a v e-type v-type]]
                (let [ref? (datascript.db/ref? db a)
                      es (get context e)
                      vs (when (and ref? (= :logic-var v-type))
                           (get context v))
                      ex-map {:e       e
                              :a       a
                              :v       v
                              :e-type  e-type
                              :v-type  v-type
                              :context context
                              :ref     ref?}]
                  (cond
                    (and (= :logic-var e-type)
                         (= :known v-type))
                    (assoc context e (eduction
                                       (map :e)
                                       (if (some? es)
                                         (eduction (mapcat #(d/datoms db :eavt % a v)) es)
                                         (do (assert-indexed db a ex-map)
                                             (d/datoms db :avet a v)))))

                    (and (= :logic-var e-type)
                         (or (= :missing v-type) (= :ignore v-type)))
                    (assoc context e (eduction
                                       (map :e)
                                       (if (some? es)
                                         (eduction (mapcat #(d/datoms db :eavt % a)) es)
                                         (d/datoms db :aevt a))))

                    (and (= :ignore e-type)
                         (= :logic-var v-type))
                    (assoc context v (eduction
                                       (map :v)
                                       (if (some? vs)
                                         (do (assert-indexed db a ex-map)
                                             (eduction (mapcat #(d/datoms db :avet a %)) vs))
                                         (d/datoms db :avet a v))))

                    (= :logic-var e-type v-type)
                    (let [datoms (condp = [(some? es) (some? vs)]
                                   [true true]
                                   (eduction (mapcat #(d/datoms db :eavt % a))
                                             (filter (comp (into #{} vs) :v))
                                             es)
                                   [true false]
                                   (eduction (mapcat #(d/datoms db :eavt % a))
                                             es)
                                   [false true]
                                   (do (assert-indexed db a ex-map)
                                       (eduction (mapcat #(d/datoms db :avet a %))
                                                 vs))
                                   [false false]
                                   (d/datoms db :avet a))]
                      (assoc context e (eduction (map :e) datoms)
                                     v (eduction (map :v) datoms)))

                    :else
                    (throw (ex-info "Unknown [e-type v-type] combination" ex-map)))))))))

;; I'm not sure where I'm at anymore.
;; If I were to implement transduce like query execution:
;; - We'd probably want to bind logic variables "as we walk the query"
;;   - Then experiment with choosing the best path.
;;   - But this is probably hard.
;;
;; What we could do instead is focus on restoring the indexed query
;; and based on what's changed, we put the changed attributes at the top
;; and reverse the where-clauses until that where-clause.
;; This should work right away, and hopefully perform somewhat good.
;; Creating a new query optimizer can be a later stage problem.
;;
;; Where we're at:
;; 1. We can index a query
;; 2. We can extract the query again (sort of).
;;
;; Next steps:
;; 3. Execute an indexed query (done~)
;; 4. Execute a re-ordered query based on changed datoms.
;; 5. Have multiple parts of a query changed (multiple attributes), re-order?, execute query.
;; 6... then we've come pretty far.

(defn extract-where-clauses [query-db query-id]
  ;; Only extracts the queries with :e :a and :v.
  ;; Why are we doing this?
  ;; It's cool and stuff, but will it be needed when we need to re-order the claues?
  ;; dunno.
  ;; Kinda fun though?
  (-> (d/q {:where '[[?query :query/id ?query-id]
                     [?query :query/map ?map]]
             :find  '[?map .]
             :in    '[$ ?query-id]}
           query-db
           query-id)
      :where))

(defn extract-find-pattern [query-db query-id]
  (let [query  (d/entity query-db [:query/id query-id])
        {:find.pattern/keys [type symbols]} (:query/find-pattern query)]
    (condp = type
      :scalar (vec (cons (first symbols) ['.]))
      :relation symbols
      :tuple [symbols]
      :collection [[(first symbols) '...]])))

(defn extract-query [query-db query-id]
  {:where (extract-where-clauses query-db query-id)
   :find  (extract-find-pattern query-db query-id)})

(defn- query-roundtrip-test* [query]
  {:pre [(map? query)]}
  (let [query-conn (d/create-conn schema)
        query-id 0]
    (d/transact! query-conn [(index-query query-id query)])
    (is (= (extract-query (d/db query-conn) query-id)
           query))))

(defspec query-roundtrip-spec
  10
  (prop/for-all [query (s/gen ::query)]
    (query-roundtrip-test* query)))

(deftest query-roundtrip-test
  (query-roundtrip-test* '{:find [(?A)], :where []}))

;; Thoughts on query engine
;; - Would have to do a recursive algorithm that binds logic-vars as it
;;   traverses the query.
;; - Need to figure out the right way to traverse multiple paths from a
;;   single logic-var.
;; - Need to be able to bind logic-vars between paths
;; - Need to be able to walk backwards

;; Now. Since query engine won't be done until later, we'll start doing
;; changed datom stuff.
;; Given some changed datoms:
;; Find queries that care
;; Bind logic-vars that matter
;; - Think about the :in part of the query.
;; Execute the query

;; <HERE>

;; Tests:
;; specify a query + pull-pattern
;; (pull (query)) should be equal to (incremental-pull (incremental-query))
;; Specify mutations to change the database
;; Eventually generate mutations to perform arbitrary changes.
;; Good strategy?

(def reads
  {:person/by-name
   {:query {:find  '[?person .]
            :where '[[?person :person/first-name ?fname]]}}
   :person/by-last-name
   {:query {:find  '[?person .]
            :where '[[?person :person/last-name ?lname]]}}
   :people/by-name
   {:query {:find  '[[?person ...]]
            :where '[[?person :person/first-name ?fname]]}}

   :people/by-param
   {:query     {:find  '[[?person ...]]
                :where '[[?person :person/first-name ?fname]]}
    :params-fn (fn [{:keys [first-name]}]
                 {'?fname first-name})}})

(s/conform ::query (-> reads :person/by-name :query))


(defn parse [db query]
  ((parser/parser {:read
                   (fn [{:keys [db] :as env} k p]
                     (let [{:keys [query pull params-fn]} (get reads k)
                           query-input (when (some? params-fn) (params-fn p))
                           query (assoc query :in (into ['$] (map key) (seq query-input)))
                           res (apply d/q query db (map val (seq query-input)))]
                       ((if (number? res) d/pull d/pull-many)
                         db (:query env) res)))})
    {:db db}
    query))

(def test-schema {:person/first-name {:db/unique :db.unique/identity}})

(defn ->tx-data-listener []
  (let [tx-data-atom (atom [])]
    (fn
      ([]
       (let [tx-data @tx-data-atom]
         (reset! tx-data-atom [])
         tx-data))
      ([tx-report]
        ;; (prn (:max-tx (:db-after tx-report)))
       (swap! tx-data-atom into (:tx-data tx-report))))))

(defn init-env [data-conn]
  (let [conn (d/create-conn schema)
        listener (->tx-data-listener)]
    (d/transact! conn (map (fn [[read-key read-map]]
                             (index-query read-key (:query read-map)))
                           reads))
    (d/listen! data-conn listener)
    {:query-conn       conn
     :query-db         (d/db conn)
     :tx-data-listener listener
     :cache            (atom {})}))

(defn incremental-pull-query [env reads parse-q]
  (let [env (update env :query-db
                    d/db-with
                    (seq (eduction
                           (map #(nth % 1))
                           (distinct)
                           (map #(hash-map :temp/changed-attr %))
                           (:tx-data env))))
        ;; instead of "execute these reads"
        ;; we've now moved the model to be
        ;; check if we should update the result.
        ;; Not sure this is a good thing.
        parser (parser/parser
                 {:read
                  (fn [{:keys [query-db db cache query]} k params]
                    (if-let [find-type (d/q {:in    '[$ ?query-id]
                                             :where '[[?query :query/id ?query-id]
                                                      [?query :query/where-clauses ?where]
                                                      [?where :where.clause/eav :a]
                                                      [?where :where.clause/value ?tx-attr]
                                                      ;; If matches any of the changed attrs, it's a match.
                                                      [_ :temp/changed-attr ?tx-attr]
                                                      [?query :query/find-pattern ?find]
                                                      [?find :find.pattern/type ?find-type]]
                                             :find  '[?find-type .]}
                                            query-db
                                            k)]
                      (let [query-params ((get-in reads [k :params-fn] (constantly nil))
                                           params)
                            res (apply
                                  d/q
                                  (update (extract-query query-db k)
                                          :in
                                          #(into (or % '[$])
                                                 (map key)
                                                 query-params))
                                  db
                                  (map val query-params))
                            pulled ((condp = find-type
                                      :scalar d/pull
                                      :collection d/pull-many
                                      (throw (ex-info (str "Cannot pull on query find-type:" find-type)
                                                      {:find-type find-type
                                                       :key       k
                                                       :query     query
                                                       :params    params})))
                                     db
                                     query
                                     res)]
                        (swap! cache assoc k pulled)
                        pulled)
                      (get @cache k)))})]
    (parser env parse-q)))

(deftest query-pull-test
  (let [data-conn (d/create-conn test-schema)
        env (init-env data-conn)]
    (testing "Cardinality one"
      (d/transact! data-conn [{:person/first-name "Petter"}])
      (let [parse-q [{:person/by-name [:person/first-name]}]
            db (d/db data-conn)]
        (is (= (parse db parse-q)
               (incremental-pull-query
                 (assoc env :db db
                            :tx-data ((:tx-data-listener env))
                            :tx-data-listener nil)
                 reads
                 parse-q))))

      (testing "Adding data to an existing entity"
        (d/transact! data-conn [{:person/first-name "Petter"
                                 :person/last-name  "Eriksson"}])
        (let [parse-q [{:person/by-name [:person/first-name]}
                       {:person/by-last-name [:person/last-name]}]
              db (d/db data-conn)]
          (is (= (parse db parse-q)
                 (incremental-pull-query
                   (assoc env :db db
                              :tx-data ((:tx-data-listener env))
                              :tx-data-listener nil)
                   reads
                   parse-q))))))

    ;; Clean up the tests also.
    (d/transact! data-conn [{:person/first-name "Diana"
                             :person/last-name  "Gren"}])
    (let [db (d/db data-conn)
          data-added ((:tx-data-listener env))]

      (testing "cardinality many"
        (let [parse-q [{:people/by-name [:person/first-name]}]]
          (is (= (parse db parse-q)
                 (incremental-pull-query
                   (assoc env :db db
                              :tx-data data-added
                              :tx-data-listener nil)
                   reads
                   parse-q)))))

      (testing "query with params"
        (let [parse-q '[({:people/by-param [:person/first-name]} {:first-name "Diana"})]]
          (is (= (parse db parse-q)
                 (incremental-pull-query
                   (assoc env :db db
                              :tx-data data-added
                              :tx-data-listener nil)
                   reads
                   parse-q))))))

    ;; Then what?
    ;; Think about re-ordering the query?

    ;; What small thing can I do to progress?
    ;; Defining a read with parameters as :symbols
    ;; Testing scalar values, sequences and maps
    ;; Testing case where attribute appears more than once
    ;; - Will need an additional test for when find pattern is a scalar.
    ;;   - Does order of which query to run matter?

    ;; Did something small (28 feb):
    ;; - data flow is just like om.next
    ;; - have something called params-fn, that returns symbols that are abound in the query.
    ;;   - actual query params should probably be banned?
    ;;   - params should only come from the environment or route-params?
    ;;     - Yes? No?
    ;; - What about separating our caching for different pull patterns?
    ;;   - Start caching entities returned from query?
    ;;   - Switch from "changed-attributes" approach to:
    ;;     - "Which queries care about these entities"?
    ;; - Also, incrementally updating pull patterns?
    ;;   - Pull pattern merging?

    ;; Wait.
    ;; What's needed to ship something?
    ;; Why do I care?
    ;; It'd be nice to have something working, so I can focus on improving something shippable.
    ;; re-ordering query is like the least needed thing for example. Ok. What do we need.

    ;; We need merging of pull patterns, so that we can return a flat map with all the reads.
    ;; - We can re-run the d/pull(-many) everytime the pull pattern for a read changes.
    ;; - Done. 5 March.
    ;; We need a better definition of reads.
    ;; - To get here, I think we need to separate the caching for pull patterns and entities?
    ;; - Incremental read is very gross.
    ;; - Switch to a "is this a changed or new datom?" thing?
    ;;   - If it's a changed, does any read care about this entity?
    ;;   - For the queries that care, can we limit the query to only focus on this datom?
    ;;   - OR: is this just an optimization?
    ;; - What about just writing the definition that I'd like for the reads?
    ;;   - Wishful thinking.
    ;;   - Maybe get "chaining reads" in there as well.
    ;; - Done.
    ;; We need to be able to "chain" reads together (for SULO).
    ;; - Done.
    ;; We need to handle "remote parsing" (with :target).
    ;; - Done.

    ;; This is it?
    ;; Integrate this version of reads with sulo?


    (testing "parameters (route-params for example)"
      ;; TODO.
      (is (= 1 1)))))

;; Let's figure out a good "definition of reads":
(def read-registry (atom {}))
(defmacro defread [k read-map]
  `(do
     (let [read-map# ~read-map
           k# ~k]
       (when (and (contains? @read-registry k#)
                  (not= (get @read-registry k#)
                        read-map#))
         (prn "WARN: read-registry already contained key: " k#
              " and it's being replaced with a new value."
              " Was: " (get @read-registry k#)
              " Replacing with: " read-map#))
       (when-not (s/valid? ::query (:query read-map#))
         (prn "WARN: :query for read: " k# " did not pass query spec.")
         (prn (s/explain-str ::query (:query read-map#))))
       (swap! read-registry assoc k# read-map#))))

(defmulti read-local (fn [env k p] k))
(defmethod read-local :default
  [env k p]
  (if-let [read-map (get @read-registry k)]
    (do 'something-maybe?)
    (throw (ex-info "read not implemented: " k))))

(defread ::people
  {:query  '{:find  [?e .]
             :where [[?e :person/first-name ?name]]}
   :sort   {:by    :person/first-name
            :order :decending}
   :params (fn [route route-params]
             (when (= :person route)
               {'?name (:name route-params)}))})
;; Implicit pull one/all when called with pull pattern.
;; - To think about when accessing the result:
;;   - If 2 component has the read, one with a pull pattern and one
;;     doesn't, the query will be deduped to contain the pull pattern
;;     so it must be possible to get the result without the implicit pull.


;; reads:
;; keyword -> map
;; Works for multimethods and maps (for testing).
;; Multi method can be wrapped in a small macro to make it look like this:
(defmulti read-local2 (fn [k] k))
(defmacro defread2 [k read-map]
  `(defmethod read-local2 ~k [_] ~read-map))

(defread ::people
  {:query  '{:find  [?e .]
             :where [[?e :person/first-name ?name]]}
   :sort   {:by    :person/first-name
            :order :decending}
   ;; I think it's fine to have route in the "env" map, since the :route key was
   ;; never used in the sulo reads.
   :params (fn [{:keys [route]} route-params]
             (when (= :person route)
               {'?name (:name route-params)}))})

;; I like this. It should do the trick.
;; I should make it easy to add before/after middleware to each/all reads/mutates.
;; It doesn't necessarily want to exist in this map.
;; Something to think about.
;; Hint: look at what middleware we had for sulo.

;; What about read dependencies though?
(defread ::addresses
  {:query      '{:find  [?e]
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
                 )})

;; Instead of having this :params fn one could do something like:
:params {'[?person ...] {:depends ::people}
         '?email        {:route-param :email}}
;; or with vectors for keys, making it possible to have any number
;; of nested keys. Making it easier to understand how it's used also.
:params {'[?person ...] [:depends ::people]
         ;; Look how easy it is to get the route now.
         '?route        [:route]
         '?email        [:route-param :email]}
;; ... That's actually quite neat.
;; Hmm.. this sugar is nice.
;; I think params should be either a map or a function.
;; In most cases, a map would be sufficient. (I think).

;; TODO: Implement something taking a read like this and executing it.

;; Thought: What if instead of "figure out which reads to run", we
;;          figure out which reads we don't need to run.
;; I like that. That way, if we can't prove that a read hasn't changed
;; we need to re-run it some way.
;; This breaks the reactive algorithm apart:
;; 1. Filter out reads that don't need to run.
;;    * Might be harder to prove that it doesn't need to be run though.
;;    * Not sure.
;; 2. Separate reads that can be optimized.
;; 3b. Run optimized reads.
;; 3a. Full re-run of remaining reads.

;; About remote data fetching:
;; We can put :remote true in the query map!!
;; The reason we didn't want to do it with om.next was because it
;; executed the read (created a value). The return from our map
;; declares what should be created and doesn't create anything.
;; So creating something that works with om.next should be pretty
;; straight forward! (jinx).
;; We just need to handle GH issue #2 for remote data stuff.

;; Another thing - Datascript/Datomic integration.
;; We should use a map of functions instead of the datascript/datomic
;; functions right away, making it easy to use either one.
;; It should also make it easier to use datomic.client.api instead of
;; datomic.api (?).

;; About getting tx-data between reads:
;; We have to keep in mind that the app-state can jump between values
;; via datascript.core/reset!. Use cases:
;; * Time travel
;; * git rebase
;; Have thought about storing the tx-data in the datascript db/conn,
;; to be consistent between these jumps. But how would the reads know
;; what has happened between their optimistic read and the new jump.
;; For example:
;; - App state: db1
;; - * remote&optimistic transaction happens *
;; - App state: db2' and db2-pending
;; - * reads read and cache db2 stuff *
;; - * Another local transaction on db2' happens *
;; - App state: db2'' and db2-pending.
;; - * Reads have now cached results for db2'' *
;; - * I'm now realizing that we could save the value of the
;;     read cache for when we git-rebase db2-pending.
;;     Such that when we finally d/reset! the app-state to db2,
;;     we can also reset the read-cache to "read-cache2", and
;;     have the tx-data from the d/transact! or d/db-with calls. *
;; - So app-state is now: [db2'' read-db2''] [db2-pending, read-db1].
;; - * result from remote query is received, creating db2 from db2-pending
;;     (which is db1). *
;; - * (let [{:keys [db-after tx-report]} (d/with db1 remote-response)
;;           ;; Create a new read-cache value with the tx-report
;;           read-cache2 (cache/with-tx-report read-cache1 tx-report)]
;;       (cb {:read-cache read-cache2
;;            :db db-after ;; <-- db2.
;;            }
;; This is interesting.
;; Just need to figure out how we get the tx-data to the cache value.

;; Read invariant:
;; The old value of the db + the tx-data should be equal to new db (db2).
;; Check should be enabled to find screw-ups.
