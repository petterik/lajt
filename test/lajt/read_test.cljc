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

(defn- ->parser []
  (fn [env query]
    {:pre [(some? (:reads env))]}
    (let [read (read/->read-fn (:reads env)
                               (lajt.read.datascript/db-fns))
          parser (parser/parser {:read read})]
      (parser env query))))

(def ^:dynamic *db*)
(def ^:dynamic *parser*)

(defn setup [test-fn]
  (binding [*db* (->db)
            *parser* (->parser)]
    (test-fn)))

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
        "Diana"))))

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

(comment
  (do
    (def ^:dynamic *db* (->db))
    (def ^:dynamic *parser* (debug-parser (->parser)))

    )
  )