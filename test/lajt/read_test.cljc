(ns lajt.read-test
  (:require
    [clojure.test :as t :refer [deftest is are testing]]
    [lajt.read :as read]
    [lajt.read.datascript]
    [lajt.parser :as parser]
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
    ([env query]
     (p (assoc env :debug true) query))))

(defn- ->parser
  ([] (->parser parser/parser))
  ([parser-fn]
   (fn [env query]
     {:pre [(some? (:reads env))]}
     (let [read (read/->read-fn (:reads env)
                                (lajt.read.datascript/db-fns))
           parser (parser-fn {:read read})]
       (parser env query)))))

(def ^:dynamic *db*)
(def ^:dynamic *parser*)

(defn setup [test-fn]
  (binding [*db* (->db)]
    (doseq [parser [(->parser parser/parser)
                    (->parser parser/eager-parser)]]
      (binding [*parser* parser]
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
                    :sort  {:order :decending}})))))

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
          #"lookup.*query"
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
                     :after [:result (partial sort-by :person/first-name) reverse]}
                    {:pull [:person/first-name]}))))

(deftest custom-action-test
  (is (= ::custom-result
         (read-query {:custom (fn [env] ::custom-result)}))))

(deftest no-op-test
  (is (nil? (read-query {:no-op true})))
  (is (nil? (read-query {:no-op true} {:pull [:something]}))))

(deftest pre-ops-for-target
  (doseq [remote [true #(-> % :params :remote?) [:params :remote?]]]
    (is (= [:read]
           (*parser* {:target :remote
                      :reads  {:read {:remote remote
                                      :params {:remote? (constantly true)}}}}
                     [:read]))))

  (testing "includes :depends-on"
    (is (= [:common-read :read1 :read2]
          (*parser* {:target :remote
                     :reads  {:common-read {:remote true}
                              :read1       {:remote     true
                                            :depends-on [:common-read]}
                              :read2       {:remote     true
                                            :depends-on [:common-read]}}}
                    [:read1 :read2])))))
(comment

  (do
    (def ^:dynamic *db* (->db))
    (def ^:dynamic *parser* (debug-parser (->parser)))

    )


  )