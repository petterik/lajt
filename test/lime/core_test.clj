(ns lime.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.spec.alpha :as s]
            [lime.core :refer :all]
            [datascript.core :as d]
            [clojure.string :as str]))

(s/def :where.clause/logic-var (s/and symbol? (comp #(str/starts-with? % "?") name)))
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


(def schema {:query/id            {:db/unique :db.unique/value}
             :query/where-clauses {:db/valueType   :db.type/ref
                                   :db/cardinality :db.cardinality/many}
             :where.clause/parts  {:db/valueType   :db.type/ref
                                   :db/cardinality :db.cardinality/many}})

(defn index-query [id query]
  (let [clauses (->> (:where query)
                     (s/conform :query/where-clauses)
                     (into []
                           (map (fn [m]
                                  {:where.clause/parts
                                   (map (fn [[k [t v :as value]]]
                                          (if (some? value)
                                            {:where.clause.part/type  t
                                             :where.clause.part/value v
                                             :where.clause.part/eav   (keyword (name k))}
                                            {:where.clause.part/eav  (keyword (name k))
                                             :where.clause.part/type :missing}))
                                        (merge #:where.clause{:e nil :a nil :v nil}
                                               m))}))))]
    [{:query/id            id
      :query/map           query
      :query/find          (:find query)
      :query/where-clauses clauses}]))

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
   :query/find          '[?e .]
   :query/where-clauses '[{:where.clause/parts [#:where.clause.part{:eav :e, :type :logic-var, :value ?e}
                                                #:where.clause.part{:eav :a, :type :known, :value :foo/bar}
                                                #:where.clause.part{:eav :v, :type :logic-var, :value ?bar}]}
                          {:where.clause/parts [#:where.clause.part{:eav :e, :type :logic-var, :value ?bar}
                                                #:where.clause.part{:eav :a, :type :known, :value :bar/baz}
                                                #:where.clause.part{:eav :v, :type :logic-var, :value ?baz}]}
                          {:where.clause/parts [#:where.clause.part{:eav :e, :type :logic-var, :value ?bar}
                                                #:where.clause.part{:eav :a, :type :known, :value :bar/xyz}
                                                #:where.clause.part{:eav :v, :type :ignore, :value _}]}
                          {:where.clause/parts [#:where.clause.part{:eav :e, :type :logic-var, :value ?bar}
                                                #:where.clause.part{:eav :a, :type :known, :value :bar/missing}
                                                #:where.clause.part{:eav :v, :type :missing}]}]})

(deftest query-indexing-test
  (let [conn (d/create-conn schema)]
    (d/transact! conn (index-query ::test-id query))
    (is (= (first (index-query ::test-id query)) expected-indexed-query))
    (is (= (d/pull (d/db conn)
                   [:query/id
                    :query/map
                    :query/find
                    {:query/where-clauses [{:where.clause/parts [:where.clause.part/value
                                                                 :where.clause.part/type
                                                                 :where.clause.part/eav]}]}]
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

