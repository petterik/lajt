(ns lajt.parser-test
  (:require
    [clojure.test :as t :refer [deftest is are testing]]
    [lajt.parser :as parser]
    [lajt.read :as read]
    [lajt.read.datascript]
    [clojure.spec.alpha :as s]
    [clojure.spec.test.alpha :as st]
    [clojure.spec.gen.alpha :as gen]))

(defn- read-mutate-handler [{:keys [query target]} k p]
  (if (some? target)
    true
    (cond-> {}
            (some? p)
            (assoc :params p)
            (some? query)
            (assoc :query query))))

(def ^:dynamic *env* {})
(def ^:dynamic *parser* nil)

(defn ->parser [parser-fn]
  (fn [env query & [target]]
    (let [read (if-let [reads (:reads env)]
                 (read/->read-fn reads (lajt.read.datascript/db-fns))
                 read-mutate-handler)
          parser (parser-fn (merge
                              {:read   read
                               :mutate read-mutate-handler}
                              env))]
      (parser env query (or target (:target env))))))

(def om-next-mutate-wrapper
  (fn [mutate]
    (fn [env k p]
      (if-some [t (:target env)]
        {t true}
        {:action (constantly
                   (mutate env k p))}))))

(defn setup [test-fn]
  (let [parser-fns [parser/parser parser/eager-parser]
        om-next-parser-fns
        (map (fn [parser-fn]
               (fn [config]
                 (parser-fn
                   (-> config
                       (update :read-plugins (fnil conj []) parser/unwrap-om-next-read-plugin)
                       (update :mutate-plugins (fnil conj []) parser/unwrap-om-next-mutate-plugin)
                       (update :read read/om-next-value-wrapper)
                       (update :mutate om-next-mutate-wrapper)))))
             parser-fns)]
    (doseq [parser-fn (concat parser-fns om-next-parser-fns)]
      (binding [*parser* (->parser parser-fn)
                s/*compile-asserts* true]
        (test-fn)))))

(t/use-fixtures :each setup)

(deftest query-parser-test
  (is (= (*parser* *env* '[:read-key
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
  (is (nil? (*parser* *env* [])))

  ;; TODO: nil results should be removed from the parsed result.
  (testing "Reading with :target"
    (let [query [:some/read
                 {:join/a [:read-key]}
                 {:union/a {:a [:read-key]}}
                 '(mutate!)
                 '(mutate! {:param 1})]]
      (is (= (set query)
             (set (*parser* *env* query :remote))))))

  (testing "removes key when parser read returns nil."
    (is (= {:read-key {}}
           (*parser* {:read (fn [env k p]
                              (when-not (= k :return-nil)
                                (read-mutate-handler env k p)))}
                   [:read-key :return-nil])))
    ;; TODO: define what happens for mutates.
    ))

(deftest recursive-dispatch-parsing-test
  (let [parser (parser/parser {:read            read-mutate-handler
                               :join-namespace  "join"
                               :union-namespace "union"
                               :union-selector  (fn [{:keys [query]} k p]
                                                  (assert (contains? query ::selected))
                                                  ::selected)})]
    (testing "joins"
      (is (= (parser *env* [{:join [{:read1 [:a :b]}
                                    {:read2 [:a :b]}]}
                            {:join/with-ns [{:read1 [:a]}]}
                            {:non-recursive [{:read1 [:a :b]}]}
                            {:non/recursive [:a]}])
             {:join          {:read1 {:query [:a :b]}
                              :read2 {:query [:a :b]}}
              :join/with-ns  {:read1 {:query [:a]}}
              :non-recursive {:query [{:read1 [:a :b]}]}
              :non/recursive {:query [:a]}})))

    (testing "unions"
      (is (= (parser *env* [{:union {::selected [:read1
                                                 {:read2 [:a]}]
                                     :other     [:read]}}
                            {:union/with-ns {::selected [:read]
                                             :other     [:read]}}
                            {:custom-union {:a [:read1]
                                            :b [:read2]}}])
             {:union         {:read1 {}
                              :read2 {:query [:a]}}
              :union/with-ns {:read {}}
              :custom-union  {:query {:a [:read1]
                                      :b [:read2]}}})))))

(deftest om-next-integration-test
  #_(let [ps (parsers {:read   (fn [env k p]
                               (if-some [t (:target env)]
                                 {t true}
                                 {:value (read-mutate-handler env k p)}))
                     :mutate (fn [env k p]
                               (if-some [t (:target env)]
                                 {t true}
                                 {:action (constantly ::executed!)}))})]
    ;; TODO?
    ))

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
  (let [parser (parser/parser {:read            read-mutate-handler
                               :join-namespace  :join
                               :union-namespace :union
                               :union-selector  (constantly ::selected)})]
    (are [query deduped] (= deduped
                            (parser/dedupe-query parser
                                                 ;; Add a parser in env, as there
                                                 ;; could be one.
                                                 (assoc *env* :parser parser)
                                                 query))
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
  (let [conf {:read            (fn [env k p] :choice)
              :join-namespace  :join
              :union-namespace :union
              :union-selector  (fn [env k p]
                                 ((:read env) env k p))}]
    (is (= [:read-key]
           (parser/dedupe-query conf *env* [{:union {:choice [:read-key]}}])))))

(deftest initializing-parser-plugins-once
  (let [state (atom 0)
        plugin {:before (fn [env k p]
                          (swap! state inc)
                          env)}]
    (*parser* (assoc *env* :read-plugins [plugin]
                           :join-namespace "join")
              [{:join [:read1 :read2 :read3]}])
    (is (contains? #{1 4} @state)))
  (let [state (atom 0)
        orig parser/recursively-call-joins-plugin]
    (with-redefs [parser/recursively-call-joins-plugin
                  (fn [namespaces]
                    {:before (fn [env k p]
                               (swap! state inc)
                               ((:before (orig namespaces)) env k p))})]
      (*parser* (assoc *env* :join-namespace "join")
                [{:join [:read1 :read2 :read3]}])
      ;; It'll be 0 for the eager-parser and 4 for the parser
      (is (contains? #{0 4} @state)))))


(comment
  (def ^:dynamic *parser* (->parser parser/parser))


  )