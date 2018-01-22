(ns lime.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.spec.alpha :as s]
            [lime.core :refer :all]
            [datascript.core :as d]
            [clojure.string :as str]))

(defn logic-var? [sym]
  (and (symbol? sym)
       (nil? (namespace sym))
       (str/starts-with? (name sym) "?")))

(s/def :where.clause/logic-var logic-var?)
(s/def :where.clause/ignore #{'_})
(s/def :where.clause/e (s/or :logic-var :where.clause/logic-var
                             :ignore :where.clause/ignore
                             :known number?))
(s/def :where.clause/a (s/or :known keyword?))
(s/def :where.clause/v (s/or
                         :logic-var :where.clause/logic-var
                         :ignore :where.clause/ignore
                         :known some?))

(s/def :query/where-clause (s/cat :where.clause/e :where.clause/e
                                  :where.clause/a (s/? :where.clause/a)
                                  :where.clause/v (s/? :where.clause/v)))
(s/def :query/where-clauses (s/coll-of :query/where-clause))

(s/def :query/find-pattern (s/or :relation (s/+ :where.clause/logic-var)
                                 :tuple (s/coll-of (s/+ :where.clause/logic-var))
                                 :scalar (s/cat :symbol :where.clause/logic-var
                                                :dot '#{.})
                                 :collection (s/coll-of (s/cat :symbol :where.clause/logic-var
                                                               :dots '#{...}))))

(s/def :query/where :query/where-clauses)
(s/def :query/find :query/find-pattern)

(s/def ::query (s/keys :req-un [:query/find :query/where]))


(def schema {:query/id            {:db/unique :db.unique/value}
             :query/where-clauses {:db/valueType   :db.type/ref
                                   :db/cardinality :db.cardinality/many}
             :query/find-pattern  {:db/valueType   :db.type/ref}
             :where.clause/parts  {:db/valueType   :db.type/ref
                                   :db/cardinality :db.cardinality/many}
             :where.clause/type   {:db/index true}
             ;:where.clause/value {:db/index true}
             :where.clause/eav    {:db/index true}
             :where.clause/idx    {:db/index true}
             :find.pattern/type   {:db/index true}
             :find.pattern/symbol {:db/index true}
             :find.pattern/index  {:db/index true}
             })

(defn- index-where-clauses [where-clauses]
  (->> (s/conform :query/where-clauses where-clauses)
       (into []
             (comp (map-indexed
                     (fn [idx m]
                       (map (fn [[k [t v :as value]]]
                              (if (some? value)
                                {:where.clause/index idx
                                 :where.clause/type  t
                                 :where.clause/value v
                                 :where.clause/eav   (keyword (name k))}
                                {:where.clause/index idx
                                 :where.clause/eav   (keyword (name k))
                                 :where.clause/type  :missing}))
                            (merge #:where.clause{:e nil :a nil :v nil}
                                   m))))
               cat))))

(s/conform :query/find-pattern '[ ?e ?d])


(defn- index-find-pattern [find-pattern]
  (let [[find-type conformed] (s/conform :query/find-pattern find-pattern)]
    (condp = find-type
      :scalar
      {:find.pattern/symbols [(:symbol conformed)]
       :find.pattern/type    find-type}

      :relation
      {:find.pattern/type   find-type
       :find.pattern/symbols conformed}

      :tuple
      {:find.pattern/type   find-type
       :find.pattern/symbols (first conformed)}

      :collection
      {:find.pattern/symbols [(-> conformed first :symbol)]
       :find.pattern/type    find-type})))

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
            [?bar :bar/missing]]})

(s/conform :query/where-clauses (:where query))

(def expected-indexed-query
  {:query/id            ::test-id
   :query/map           {:find  '[?e .]
                         :where '[[?e :foo/bar ?bar]
                                  [?bar :bar/baz ?baz]
                                  [?bar :bar/xyz _]
                                  [?bar :bar/missing]]}
   ;; We may want to index/parse :query/find more than just getting the raw pattern.
   :query/find-pattern  {:find.pattern/type    :scalar
                         :find.pattern/symbols ['?e]}
   :query/where-clauses '[#:where.clause{:index 0 :eav :e, :type :logic-var, :value ?e}
                          #:where.clause{:index 0 :eav :a, :type :known, :value :foo/bar}
                          #:where.clause{:index 0 :eav :v, :type :logic-var, :value ?bar}
                          #:where.clause{:index 1 :eav :e, :type :logic-var, :value ?bar}
                          #:where.clause{:index 1 :eav :a, :type :known, :value :bar/baz}
                          #:where.clause{:index 1 :eav :v, :type :logic-var, :value ?baz}
                          #:where.clause{:index 2 :eav :e, :type :logic-var, :value ?bar}
                          #:where.clause{:index 2 :eav :a, :type :known, :value :bar/xyz}
                          #:where.clause{:index 2 :eav :v, :type :ignore, :value _}
                          #:where.clause{:index 3 :eav :e, :type :logic-var, :value ?bar}
                          #:where.clause{:index 3 :eav :a, :type :known, :value :bar/missing}
                          #:where.clause{:index 3 :eav :v, :type :missing}]})

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
                    {:query/where-clauses [:where.clause/index
                                           :where.clause/value
                                           :where.clause/type
                                           :where.clause/eav]}]
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

    (is (= (d/entid (d/db data-conn) [:user/name "Petter"])
           (first ((query-xf (d/db query-conn) 0) (d/db data-conn)))))
    [data-conn query-conn queries])

  (def setup *1)
  (def data-conn (first setup))
  (def query-conn (second setup))
  (def queries (nth setup 2))

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
                 ;; Would be nice to have an indexed where.clause/symbol here.
                 ;; Instead of first filtering by :logic-var then getting the value.
                 [?where :where.clause/type :logic-var]
                 [?where :where.clause/value ?out-symbol]
                 [?where :where.clause/eav ?eav]]
        :find '[?out-symbol ?eav]})

  ;; TODO: more.
  ;; Thought process:
  ;; Given a query-id and an [?out-symbol ...]
  ;; - Output filters and additional paths to new out-symbols (paths via references).
  ;;   - Filters when the :where.clause/type is :known
  ;;   - Output new symbols to follow when the other (:e or :v) is :logic-var.
  ;; Then we'll create a transducer given the output of this ^^^^ function.

  ;; To be continued...

  )