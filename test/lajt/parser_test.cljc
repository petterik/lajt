(ns lajt.parser-test
  (:require
    [clojure.test :as t :refer [deftest is are testing]]
    [lajt.parser :as parser]
    [lajt.read :as read]
    [lajt.read.datascript]
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.test.check.properties :as prop :include-macros true]
    [clojure.test.check.clojure-test :refer [defspec] :include-macros true]))

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
  (let [om-next-parser (fn [config]
                            (parser/parser
                              (-> config
                                  (update :read-plugins (fnil conj []) parser/unwrap-om-next-read-plugin)
                                  (update :mutate-plugins (fnil conj []) parser/unwrap-om-next-mutate-plugin)
                                  (update :read read/om-next-value-wrapper)
                                  (update :mutate om-next-mutate-wrapper))))
        parser-fns [parser/parser parser/eager-parser om-next-parser]]
    (doseq [parser-fn parser-fns]
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

(deftest query-merging

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
        (mapv #(hash-map % [:x]) keyword-vec)))))

(deftest dedupe-query-test
  (are [query deduped] (= deduped (parser/dedupe-query
                                    {:join-namespaces  [:join]
                                     :union-namespaces [:union]
                                     :union-selector   (constantly ::selected)}
                                    *env*
                                    query))
    [{:join [:read]} {:read [:a]} :read]
    [{:read [:a]}]

    [:read]
    [:read]

    [{:read [:a]} :read]
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
      :read2]

    [{:non-selected/union {:a [:read :read2]}}
     {:non-selected/union {:a [{:read [:b]} :read3]}}]
    [{:non-selected/union {:a [{:read [:b]} :read2 :read3]}}]

    [{:non-selected/union {:a [:read]}}
     {:non-selected/union {:b [:read]}}]
    [{:non-selected/union {:a [:read]
                           :b [:read]}}])

  (testing "multiple union selections"
    (is (= [{:read [:a :b]}]
           (->> [{:union {:a [{:read [:a]}]
                          :b [{:read [:b]}]
                          :c [{:read [:c]}]}}]
                (parser/dedupe-query {:union-namespaces [:union]
                                      :union-selector   (fn [_ _ _] [:a :b])}
                                     *env*)))))

  (testing "union selections with no query is filtered out"
    (is (= [:read]
           (->> [{:union {:a [:read]}}]
                (parser/dedupe-query {:union-namespaces [:union]
                                      :union-selector   (constantly [:a :b])}
                                     *env*)))))

  ;; Ok, this is cool and all. But what happens when one parses a
  ;; query with the same read that has different params?
  '[({:read [:a]} {:param 1})
    ({:read [:a]} {:param 2})]
  ;; Should only be a problem for remote queries?
  ;; (as query params are banned for local reads).
  ;; Created https://github.com/petterik/lajt/issues/2
  )

(defspec parsed-query-roundtrip
  50
  (prop/for-all [query (s/gen ::parser/query)]
    (= query
       (->> query
            (#'parser/query->parsed-query)
            (#'parser/parsed-query->query)))))

(deftest query->parsed-query-roundtrips
  (are [query] (= query (->> query
                             (#'parser/query->parsed-query)
                             (#'parser/parsed-query->query)))
    '[{:A 0}]
    '[(A)]
    '([:read] {:param 1})))

(deftest query-params-are-passed-to-env-test
  (is (= (*parser* (assoc *env* :read (fn [env k p] (:query-params env)))
                 '([:read-key] {:query-params ::param-value}))
         {:read-key ::param-value})))

(defspec update-query-roundtrip
  30
  (prop/for-all [query (s/gen ::parser/query)]
    (= query (parser/update-query (map identity) query))))

(defspec update-query-params-roundtrip
  30
  (prop/for-all [query (s/gen ::parser/query)
                 m (s/gen map?)]
    (let [params (parser/get-query-params
                    (parser/update-query-params query (constantly m)))]
      (if (s/valid? ::parser/query-param-expr query)
        (= m params)
        (nil? params)))))

(defspec query-into-test
  30
  (prop/for-all [query-keys (s/gen (s/coll-of keyword? :kind vector?))
                 params (s/gen (s/nilable map?))]
    (let [query (cond-> query-keys
                        (and (seq query-keys) params)
                        (list params))]
      (and (s/valid? ::parser/query query)
        (= (parser/query-into #{} (map ::parser/key) query)
           (set query-keys))))))

(comment
  (def ^:dynamic *parser* (->parser parser/parser))

  (def query '[{:A 0}])

  (def rountrip *1)
  )