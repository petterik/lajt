(ns lajt.read-test
  (:require
    [clojure.test :as t :refer [deftest is are testing]]
    [lajt.read :as read]
    [lajt.read.datascript]
    [lajt.parser :as parser]
    [lajt.parser-test :as parser-test]
    [datascript.core :as d]))

;; Ok cool. Reads implemented.
;; Should work in SULO now?
;; Could write some tests to see if it works for the cases we want to implement.
;; Once it works, we can add incremental stuff.

;; It'd be nice to have a test with om.next to run via the new cljs.main stuff?
;; Something that

(defn- ->db []
  (-> (d/create-conn {:person/first-name {:db/unique :db.unique/identity}})
      (d/db)
      (d/db-with [{:person/first-name "Petter"}
                  {:person/first-name "Diana"}])))

(defn debug-parser [p]
  (fn
    ([] (p))
    ([env query & [target]]
     (p (assoc env :debug true) query (or target (:target env))))))

(def ^:dynamic *db*)
(def ^:dynamic *parser*)

(defn setup [test-fn]
  (parser-test/setup
    (fn []
      (binding [*db* (->db)
                *parser* parser-test/*parser*]
        (test-fn)))))

(t/use-fixtures :each setup)

(defn read-query
  ([read]
   (read-query read nil))
  ([read {:keys [pull] :as env}]
   (let [key ::test-read-key
         result (*parser*
                  (merge {:db    *db*
                          :reads {key read}}
                         (dissoc env :pull))
                  (if (some? pull)
                    [{key pull}]
                    [key]))]
     (get result key))))

(deftest simple-reads
  (let [person-by-name
        {:query  '{:find  [?e .]
                   :where [[?e :person/first-name ?name]]}
         :params {'?name [:route-params :name]}}]
    (testing "reads with pull pattern"
      (is (= #{{:person/first-name "Petter"}
               {:person/first-name "Diana"}}
             (set (read-query
                    {:query '{:find  [[?e ...]]
                              :where [[?e :person/first-name]]}}
                    {:pull [:person/first-name]}))))
      (is (= {:person/first-name "Petter"}
             (read-query
               person-by-name
               {:pull [:person/first-name]
                :route-params {:name "Petter"}}))))

    (testing "read without query returns whatever the query returned."
      (is (= (d/entid *db* [:person/first-name "Petter"])
             (read-query person-by-name
                         {:route-params {:name "Petter"}})))
      (is (= #{"Petter" "Diana"}
             (set
               (read-query
                 {:query '{:find  [[?name ...]]
                           :where [[_ :person/first-name ?name]]}}))))
      (is (= ["Petter" "Diana"]
             (read-query
               {:query '{:find  [[?name ...]]
                         :where [[_ :person/first-name ?name]]}
                :sort  {:order :decending}}))))))

(deftest reads-with-order
  (testing ":order with query"
    (is (= [{:person/first-name "Diana"}
            {:person/first-name "Petter"}]
           (read-query
             {:query '{:find  [[?e ...]]
                       :where [[?e :person/first-name]]}
              :sort  {:key-fn :person/first-name}}
             {:pull [:person/first-name]})
           ))
    (is (= [{:person/first-name "Petter"}
            {:person/first-name "Diana"}]
           (read-query
             {:query '{:find  [[?e ...]]
                       :where [[?e :person/first-name]]}
              :sort  {:key-fn :person/first-name
                      :order  :decending}}
             {:pull [:person/first-name]}))))

  (testing ":order without query"
    (is (apply < (read-query
                   {:query '{:find  [[?e ...]]
                             :where [[?e :person/first-name]]}
                    :sort  {}})))

    (is (apply > (read-query
                   {:query '{:find  [[?e ...]]
                             :where [[?e :person/first-name]]}
                    :sort  {:order :decending}}))))
  (testing ":sort-key gets put in the remote pull-pattern"
    (let [reads {:read {:query  '{:find  [[?e ...]]
                                  :where [[?e :person/first-name]]}
                        :sort   {:key-fn :person/last-name}
                        :remote true}}
          op-deps {[:sort :lajt.op.stage/remote]
                   [[:query :lajt.op.stage/remote]]}]
      ;; Gets :person/first-name too because it's in the query
      ;; and we've now added this feature.
      (is (= [{:read [:person/first-name :person/last-name]}]
             (*parser* {:reads                   reads
                        :target                  :remote
                        ::read/op-dependency-map op-deps}
                       [:read])))
      (is (= [{:read [:person/first-name :person/last-name]}]
             (*parser* {:reads                   reads
                        :target                  :remote
                        ::read/op-dependency-map op-deps}
                       [{:read [:person/first-name]}]))))))

(deftest reads-depending-on-other-reads
  (let [reads
        {:names/any-name
         {:query '{:find   [?name .]
                   :where  [[_ :person/first-name ?name]]}
          :params {'?name [:route-params :name]}}
         :person/by-any-name
         {:query      '{:find   [?e .]
                        :where  [[?e :person/first-name ?name]]}
          :params {'?name [:depends-on :names/any-name]}
          :depends-on [:names/any-name]}}]
    (testing ":depends-on"
      (are [name] (= name
                     (:names/any-name
                       (*parser* {:db           *db*
                                  :reads        reads
                                  :route-params {:name name}}
                                 [:names/any-name])))
        "Petter"
        "Diana")
      (are [name] (= {:person/first-name name}
                     (:person/by-any-name
                       (*parser* {:db           *db*
                                  :reads        reads
                                  :route-params {:name name}}
                                 [{:person/by-any-name [:person/first-name]}])))
        "Petter"
        "Diana"))
    (testing ":depends-on can be a function"
      (let [name "Petter"
            reads (update-in reads [:person/by-any-name :depends-on]
                             (fn [deps]
                               (fn [env]
                                 deps)))]
        (is (= {:person/first-name name}
               (:person/by-any-name
                 (*parser* {:db           *db*
                            :reads        reads
                            :route-params {:name name}}
                           [{:person/by-any-name [:person/first-name]}]))))))

    (testing ":depends-on can take a pull-pattern"
      (let [reads
            {:people
             {:query  '{:find  [[?e ...]]
                        :where [[?e :person/first-name ?name]]}
              :params {'?name [:route-params :name]}}
             :person/by-any-name
             {:query      '{:find  [?e .]
                            :where [[?e :person/first-name ?name]]}
              :params     {'[[[_ ?name]] ...] [:depends-on :people]}
              :depends-on [{:people [:person/first-name]}]}}
            name "Petter"]
        (is (= {:person/first-name name}
               (:person/by-any-name
                 (*parser* {:db           *db*
                            :reads        reads
                            :route-params {:name name}}
                           [{:person/by-any-name [:person/first-name]}]))))))))

(deftest reads-with-a-before-function
  (testing ":before function gets run before the query"
    (let [state (atom 0)]
      ;; :atom gets incremented by one in the :case/before-fn.
      (*parser* {:db    *db*
                 :reads {:case/before-fn
                         {:before (fn [env]
                                    (swap! (:atom env) inc))
                          :query  '{:find  [?e .]
                                    :where [[?e :person/first-name]]}}}
                 :atom  state} [:case/before-fn])
      (is (== 1 @state)))))

(deftest reads-by-lookup-ref
  (testing "reads by lookup-ref"
    (is (= (d/q '{:find  [?e .]
                  :where [[?e :person/first-name "Petter"]]}
                *db*)
           (read-query {:lookup-ref [:person/first-name "Petter"]})))
    (is (= (d/q '{:find  [?e .]
                  :where [[?e :person/first-name "Diana"]]}
                *db*)
           (read-query {:lookup-ref [:person/first-name "Diana"]})))

    (testing "can pull on lookup-refs"
      (is (= {:person/first-name "Petter"}
             (read-query {:lookup-ref [:person/first-name "Petter"]}
                         {:pull [:person/first-name]})))
      (is (= {:person/first-name "Diana"}
             (read-query {:lookup-ref [:person/first-name "Diana"]}
                         {:pull [:person/first-name]})))))

  (testing "cannot have both lookup ref and query"
    (is (thrown-with-msg?
          #?(:clj Exception :cljs js/Error)
          #"multiple.*actions"
          (*parser*
            {:db    *db*
             :reads {:crash/lookup+query
                     ;; Cannot have both lookup ref and query
                     {:lookup-ref [:person/first-name "Petter"]
                      :query      '{:find  [?e .]
                                    :where [[?e :person/first-name]]}}}}
            [:crash/lookup+query])))))

(deftest reading-non-existent-key-throws
  (testing "Calls on key that does not exist, throws"
    (is (thrown-with-msg?
          #?(:clj Exception :cljs js/Error)
          #"[Nn]o such read"
          (*parser* {:db    *db*
                     :reads {}}
                    [:kdsoakdosa/kdsoakdsa])))))

(deftest query-with-nil-params-is-nil
  (is (nil? (read-query {:query  '{:find [?e .]}
                         :params {'?e nil}})))
  (is (nil? (read-query {:query  '{:find  [?e .]
                                   :where [[?e :person/first-name ?name]]}
                         :params {'?name nil}})))
  (is (nil? (read-query {:query  '{:find  [?e .]
                                   :where [[?e :person/first-name ?name]]}
                         :params {'?name nil}}
                        {:pull [:person/first-name]}))))

(deftest multiple-reads-test
  (let [reads
        {:nil-params
         {:query  '{:find  [?name .]
                    :where [[?e :person/first-name ?name]]}
          :params {'?name nil}}
         :diana
         {:query  '{:find  [?name .]
                    :where [[?e :person/first-name ?name]]}
          :params {'?name (constantly "Diana")}}
         :petter
         {:query  '{:find  [?name .]
                    :where [[?e :person/first-name ?name]]}
          :params {'?name (constantly "Petter")}}}]
    (is (= {:diana "Diana"
            :petter "Petter"}
           (*parser* {:db    *db*
                      :reads reads}
                     [:nil-params :diana :petter])))))

(deftest case-queries
  (let [query {:base {:query '{:find  [?name .]
                               :where [[?e :person/first-name ?name]]}}
               :case [{[[:route-params :petter]]
                       {:query '{:where [[?e :person/first-name "Petter"]]}}}

                      {[[:route-params :diana]]
                       {:query '{:where [[?e :person/first-name "Diana"]]}}}]}]
    (is (= "Petter" (read-query query {:route-params {:petter "any"}})))
    (is (= "Diana" (read-query query {:route-params {:diana "any"}})))
    ;; Selects "Petter" first because order matters.
    (is (= "Petter" (read-query query {:route-params {:diana "any"
                                                      :petter "any"}})))
    (is (nil? (read-query query {:route-params nil})))))

(deftest function-in-find-pattern-test
  (is (= 2 (read-query {:query '{:find  [(count ?e) .]
                                 :where [[?e :person/first-name]]}})))
  (is (= #{{:person/first-name "Petter"}
           {:person/first-name "Diana"}}
         (set
           (read-query {:query '{:find  [(distinct ?e) .]
                                 :where [[?e :person/first-name]]}}
                       {:pull [:person/first-name]})))))

(deftest after-test
  (is (= [{:person/first-name "Petter"}
          {:person/first-name "Diana"}]
        (read-query {:query '{:find  [[?e ...]]
                              :where [[?e :person/first-name]]}
                     :lastly [:result (partial sort-by :person/first-name) reverse]}
                    {:pull [:person/first-name]}))))

(deftest custom-action-test
  (is (= ::custom-result
         (read-query {:custom (fn [env] ::custom-result)}))))

(deftest no-op-test
  (is (nil? (read-query {:no-op true})))
  (is (nil? (read-query {:no-op true} {:pull [:something]})))
  (is (= [:read] (*parser* {:reads {:read {:remote true :no-op true}}}
                         [:read]
                         :remote)))
  (is (= '[(:read {:foo 1})]
         (*parser* {:reads {:read {:remote true :no-op true}}}
                   '[(:read {:foo 1})]
                   :remote)))
  (is (= '[:read :read]
         (*parser* {:reads {:read {:remote true :no-op true}}}
                   '[:read :read]
                   :remote))))

(deftest pre-ops-for-target
  (doseq [remote [true #(-> % :params :remote?) [:params :remote?]]]
    (is (= [:read]
           (*parser* {:target :remote
                      :reads  {:read {:remote remote
                                      :params {:remote? (constantly true)}}}}
                     [:read]))))
  (is (= []
         (*parser* {:target :remote
                    :reads  {:read {:remote (constantly false)}}}
                   [:read])))

  (testing "includes :depends-on"
    (is (= (set [:common-read :read1 :read2])
           (set (*parser* {:target :remote
                           :reads  {:common-read {:remote true}
                                    :read1       {:remote     true
                                                  :depends-on [:common-read]}
                                    :read2       {:remote     true
                                                  :depends-on [:common-read]}}}
                          [:read1 :read2]))))

    (is (= [{:dep [:read-key]}]
           (*parser* {:target :remote
                      :reads  {:dep {:remote true}
                               :read1       {:remote     false
                                             :depends-on [{:dep [:read-key]}]}}}
                     [:read1]))))

  (testing "gets :target from :base even if no :case is true."
    (is (= [:read1]
           (*parser* {:target :remote
                      :reads  {:read1 {:base {:remote true}
                                       :case [{(constantly false) {}}]}}}
                     [:read1]))))
  (testing ":target can return a new read to read"
    (is (= [:read2]
           (*parser* {:target :remote
                      :reads  {:read1 {:remote (fn [env] [:read2])}}}
                     [:read1])))
    (is (= [{:read2 [:with-query]}]
           (*parser* {:target :remote
                      :reads  {:read1 {:remote (fn [env] [{:read2 [:with-query]}])}}}
                     [:read1])))
    (is (= ['({:read2 [:read-key]} {:params 1})]
           (*parser* {:target :remote
                      :reads  {:read1 {:remote (fn [env]
                                                 ['({:read2 [:read-key]}
                                                     {:params 1})])}}}
                     [:read1])))

    (testing "Can include true"
      (is (= [:read2 :read1]
             (*parser* {:target :remote
                        :reads  {:read1 {:remote (fn [env] [:read2 true])}}}
                       [:read1]))))))

(deftest query-returns-pull-pattern-for-remote-targets
  (are [find-pattern pull-pattern]
       (= pull-pattern
          (*parser* {:target :remote
                     :reads  {:read1
                              {:remote true
                               :query  {:find  find-pattern
                                        :where '[[?e :person/foo ?foo]
                                                 [?e :person/q]
                                                 [?bar :bar/people ?e]
                                                 [?foo :foo/foos]
                                                 [?bar :bar/xyz ?xyz]
                                                 [?xyz :xyz/abc ?abc]]}}}}
                    [{:read1 [:person/first-name]}]))
    '[[?e ...]]
    [{:read1 [:person/first-name
              {:person/foo [:foo/foos]}
              :person/q
              {:bar/_people [{:bar/xyz [:xyz/abc]}]}]}]

    '[?e .]
    [{:read1 [:person/first-name
              {:person/foo [:foo/foos]}
              :person/q
              {:bar/_people [{:bar/xyz [:xyz/abc]}]}]}]

    ;; relations doesn't get anything more than they asked for.
    ;; Maybe we should not allow relations or tuples?
    '[?e ?foo]
    [{:read1 [:person/first-name]}])

  (testing "functions are skipped, when creating pull pattern."
    (is (= [{:read1 [:person/first-name
                     :person/foo]}]
           (*parser* {:target :remote
                      :reads {:read1
                              {:remote true
                               :query '{:find [?e .]
                                        :where [[?e :person/first-name]
                                                [(some-fn ?e) ?b]
                                                [?e :person/foo]]}}}}
                     [:read1]))))

  (testing ":depends-on gets its :query turned into a pull pattern."
    (is (= [{:read2 [:person/first-name]}]
           (*parser* {:target :remote
                      :reads  {:read1
                               {:no-op      {}
                                :depends-on [:read2]}
                               :read2
                               {:query '{:find [?e .]
                                         :where [[?e :person/first-name]]}}}}
                     [:read1])))))

(deftest lookup-ref-keyword-gets-added-to-pull-pattern
  (testing "Gets added when there's already a pull pattern"
    (is (= [{:lookup [:a :unique-key]}]
          (*parser* {:target :remote
                     :reads  {:lookup
                              {:remote     true
                               :lookup-ref [:unique-key :unique-value]}}}
                    [{:lookup [:a]}]))))
  ;; TODO: Figure out if this is a good idea
  (testing "does not get added when there's no pull pattern"
    (is (= [:lookup]
           (*parser* {:target :remote
                      :reads  {:lookup
                               {:remote     true
                                :lookup-ref [:unique-key :unique-value]}}}
                     [:lookup]))))
  (testing "does not get added when :remote is false"
    (is (= []
           (*parser* {:target :remote
                      :reads {:lookup {:lookup-ref [:k :v]}}}
                     [{:lookup [:a]}])))))

(comment

  (do
    (def ^:dynamic *db* (->db))
    (def ^:dynamic *parser* (debug-parser (parser-test/->parser
                                            parser/parser)))

    )
  (clojure.spec.test.alpha/unstrument)

  )