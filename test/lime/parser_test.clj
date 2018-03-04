(ns lime.parser-test
  (:require
    [clojure.test :as t :refer [deftest is are testing]]
    [lime.parser :as parser]
    [clojure.spec.alpha :as s]))

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
                      (mutate-with-params {:param 1})])
         '{:read-key          {}
           :join/a            {:query [:read-key]}
           :join/b            {:query [{:join/a [:read-key]}]}
           :union/a           {:query {:union.a/x [:read-key]
                                       :union.a/y [{:join/a [:read-key]}]}}
           :read-key2         {:params {:param 1}}
           :join/c            {:query [:read-key] :params {:param 1}}
           :union/b           {:query {:union.b/a [:read-key]} :params {:param 1}}
           mutate-no-params   {}
           mutate-with-params {:params {:param 1}}})))

(deftest query-parser-test
  (binding [s/*compile-asserts* true]
    (parse-test-query
      (parser/parser {:mutate read-mutate-handler
                      :read   read-mutate-handler}))
    (parse-test-query
      (parser/lazy-parser {:mutate read-mutate-handler
                           :read   read-mutate-handler}))))

(deftest recursive-dispatch-parsing-test
  (let [parser (parser/lazy-parser {:read read-mutate-handler
                                    :join-namespace "join"
                                    :union-namespace "union"
                                    :union-selector (fn [{:keys [query]} k p]
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

