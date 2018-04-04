(ns lajt.parser-test
  (:require
    [clojure.test :as t :refer [deftest is are testing]]
    [lajt.parser :as parser]
    [clojure.spec.alpha :as s]
    [clojure.spec.test.alpha :as st]
    [clojure.spec.gen.alpha :as gen]))

(defn- read-mutate-handler [{:keys [query]} k p]
  (cond-> {}
          (some? p)
          (assoc :params p)
          (some? query)
          (assoc :query query)))

(defn- parse-test-query [parser]
  (is (= (parser {} '[:read-key
                      {:join/a [:read-key]}
                      {:join/b [{:join/a [:read-key]}]}
                      {:union/a {:union.a/x [:read-key]
                                 :union.a/y [{:join/a [:read-key]}]}}
                      (:read-key2 {:param 1})
                      ({:join/c [:read-key]} {:param 1})
                      ({:union/b {:union.b/a [:read-key]}} {:param 1})
                      (mutate-no-params)
                      (mutate-with-params {:param 1})
                      {:join/recursive [:read-key {:recur ...}]}])
         '{:read-key          {}
           :join/a            {:query [:read-key]}
           :join/b            {:query [{:join/a [:read-key]}]}
           :union/a           {:query {:union.a/x [:read-key]
                                       :union.a/y [{:join/a [:read-key]}]}}
           :read-key2         {:params {:param 1}}
           :join/c            {:query [:read-key] :params {:param 1}}
           :union/b           {:query {:union.b/a [:read-key]} :params {:param 1}}
           :join/recursive    {:query [:read-key {:recur ...}]}
           mutate-no-params   {}
           mutate-with-params {:params {:param 1}}}))
  (is (nil? (parser {} []))))

(deftest query-parser-test
  (binding [s/*compile-asserts* true]
    (parse-test-query
      (parser/parser {:mutate read-mutate-handler
                      :read   read-mutate-handler}))
    (parse-test-query
      (parser/eager-parser {:mutate read-mutate-handler
                            :read   read-mutate-handler}))))

(deftest recursive-dispatch-parsing-test
  (let [parser (parser/parser {:read                 read-mutate-handler
                                    :join-namespace  "join"
                                    :union-namespace "union"
                                    :union-selector  (fn [{:keys [query]} k p]
                                                      (assert (contains? query ::selected))
                                                      ::selected)})]
    (testing "joins"
      (is (= (parser {} [{:join [{:read1 [:a :b]}
                                 {:read2 [:a :b]}]}
                         {:join/with-ns [{:read1 [:a]}]}
                         {:non-recursive [{:read1 [:a :b]}]}
                         {:non/recursive [:a]}])
             {:join {:read1 {:query [:a :b]}
                     :read2 {:query [:a :b]}}
              :join/with-ns {:read1 {:query [:a]}}
              :non-recursive {:query [{:read1 [:a :b]}]}
              :non/recursive {:query [:a]}})))

    (testing "unions"
      (is (= (parser {} [{:union {::selected [:read1
                                              {:read2 [:a]}]
                                  :other [:read]}}
                         {:union/with-ns {::selected [:read]
                                          :other [:read]}}
                         {:custom-union {:a [:read1]
                                         :b [:read2]}}])
             {:union {:read1 {}
                      :read2 {:query [:a]}}
              :union/with-ns {:read {}}
              :custom-union {:query {:a [:read1]
                                     :b [:read2]}}})))))

(deftest query-merging
  (are [query pattern-map] (= pattern-map
                              (parser/query->pattern-map query))
    [:a :b] {:a nil :b nil}

    [{:read-key [:a]}] {:read-key {:a nil}}
    [{:read-key [:a]} {:read-key [:b]}] {:read-key {:a nil :b nil}}

    [{:read-key [:a]} {:read-key [{:a [:x]}]}] {:read-key {:a {:x nil}}}
    [{:read-key [{:a [:y]}]} {:read-key [{:a [:x]}]}] {:read-key {:a {:x nil :y nil}}}
    [{:read-key [{:a [:x :y]}]} {:read-key [{:a [:x]}]}] {:read-key {:a {:x nil :y nil}}}

    ;; Params
    '[(:read-key {:param 1})] {[:read-key {:param 1}] nil}
    '[({:read-key [:a]} {:param 1})] {[:read-key {:param 1}] {:a nil}}
    '[({:read-key [:a :b]} {:param 1})
      ({:read-key [{:a [:x]}]} {:param 1})] {[:read-key {:param 1}] {:a {:x nil} :b nil}}

    ;; Different params don't merge
    '[({:read-key [:a]} {:param 1})
      ({:read-key [:a]} {:param 2})] {[:read-key {:param 1}] {:a nil} [:read-key {:param 2}] {:a nil}})

  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"Unions.*not allowed"
                        (parser/query->pattern-map [{:union {:a [:b]}}])))


  (are [pattern-map query] (= query (parser/pattern-map->query pattern-map))
    {:read-key nil} [:read-key]
    {:read-key {:a nil}} [{:read-key [:a]}]

    {:read-key {:a nil :b nil}} [{:read-key [:a :b]}]

    ;; Params
    {[:read-key {:param 1}] nil} '[(:read-key {:param 1})]
    {[:read-key {:param 1}] {:a nil}
     [:read-key {:param 2}] {:a nil}} '[({:read-key [:a]} {:param 1}) ({:read-key [:a]} {:param 2})]
    )

  (are [query merged] (= merged (parser/merge-read-queries query))
    [:a :b] [:a :b]

    [{:read [:a]}
     {:read [:b]}]
    [{:read [:a :b]}]

    '[({:read-key [:a :b]} {:param 1})
      ({:read-key [{:a [:x]}]} {:param 1})]
    '[({:read-key [{:a [:x]} :b]} {:param 1})]

    '[({:read-key [:a]} {:param 1})
      ({:read-key [:a]} {:param 2})]
    '[({:read-key [:a]} {:param 1}) ({:read-key [:a]} {:param 2})])

  ;; Keeping order of the pattern is nice to have.
  (let [keyword-vec [:a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p]]
    (testing "keeping order of the query"
      (are [query merged] (= merged (parser/merge-read-queries query))
        keyword-vec
        keyword-vec

        ;; Joins
        (mapv #(hash-map % [:x]) keyword-vec)
        (mapv #(hash-map % [:x]) keyword-vec)

        ;; Merged joins with enough entries to cause un-ordered maps.
        (->> (cycle keyword-vec)
             (take 100)
             (map-indexed (fn [i k]
                            {k [(keyword (str i))]}))
             (vec))

        ;; Should result in:
        ;; [{:a [:0 :16 :32 :48 :64 :80 :96]}
        ;; {:b [:1 :17 :33 :49 :65 :81 :97]}
        ;; {:c [:2 :18 :34 :50 :66 :82 :98]}
        ;; {:d [:3 :19 :35 :51 :67 :83 :99]}
        ;; {:e [:4 :20 :36 :52 :68 :84]}
        ;; ... etc
        ;; ]
        (->> (cycle keyword-vec)
             (take 100)
             (map-indexed (fn [i k]
                            {k [(keyword (str i))]}))
             (apply parser/merge-ordered-with (fn [a b]
                                                (into (cond-> a (number? a) [a])
                                                      (cond-> b (number? b) [b]))))
             (map (fn [[k v]]
                    {k v})))))))

(deftest dedupe-query-test
  (let [parser (parser/parser {:read                 read-mutate-handler
                                    :join-namespace  :join
                                    :union-namespace :union
                                    :union-selector  (constantly ::selected)})]
    (are [query deduped] (= deduped
                            (parser/dedupe-query parser {} query))
      [{:join [:read]} {:read [:a]}]
      [{:read [:a]}]

      ;; unions, mutations and nested joins. Complex stuff.
      '[{:read [:a {:b [:x]}]}
        {:union {::selected [{:read [:c]}
                             :read2]}}
        (foo {:bar 1})
        {:join/a [{:join/b [{:read [{:a [:x]} {:b [:y]} :d]}]}]}]

      '[(foo {:bar 1})
        {:read [{:a [:x]}
                {:b [:x :y]}
                :c
                :d]}
        :read2])

    ;; Ok, this is cool and all. But what happens when one parses a
    ;; query with the same read that has different params?
    '[({:read [:a]} {:param 1})
      ({:read [:a]} {:param 2})]
    ;; Should only be a problem for remote queries?
    ;; (as query params are banned for local reads).
    ;; Created https://github.com/petterik/lajt/issues/2
    ))

(deftest dispatch-old-read-test
  (let [read (fn [env k p] :choice)
        conf {:read            read
              :join-namespace  :join
              :union-namespace :union
              :union-selector  (fn [env k p]
                                 (prn "in union selector")
                                 (doto ((:read env) env k p) prn))}]
    (is (= [:read-key]
           (parser/dedupe-query conf {} [{:union {:choice [:read-key]}}])))))