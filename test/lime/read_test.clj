(ns lime.read-test
  (:require
    [clojure.test :refer [deftest is are testing]]
    [lime.read :as read]
    [lime.read.datascript]
    [lime.parser :as parser]
    [datascript.core :as d]))

;; Ok cool. Reads implemented.
;; Should work in SULO now?
;; Could write some tests to see if it works for the cases we want to implement.
;; Once it works, we can add incremental stuff.

;; It'd be nice to have a test with om.next to run via the new cljs.main stuff?
;; Something that

(def reads {:people
            {:query '{:find  [[?e ...]]
                      :where [[?e :person/first-name]]}}
            :person/by-name
            {:query  '{:find  [?e .]
                       :where [[?e :person/first-name ?name]]}
             :params {'?name [:route-params :name]}}
            :people/names
            {:query '{:find  [[?name ...]]
                      :where [[_ :person/first-name ?name]]}}

            ;; Order by names
            :people/names-decending
            {:query '{:find  [[?name ...]]
                      :where [[_ :person/first-name ?name]]}
             :sort  {:order :decending}}
            :people/order-name-accending
            {:query '{:find  [[?e ...]]
                      :where [[?e :person/first-name]]}
             :sort  {:key-fn :person/first-name}}
            :people/order-name-decending
            {:query '{:find  [[?e ...]]
                      :where [[?e :person/first-name]]}
             :sort  {:key-fn :person/first-name
                     :order  :decending}}
            ;; Order by eid
            :people/order-eid-accending
            {:query '{:find  [[?e ...]]
                      :where [[?e :person/first-name]]}
             :sort  {}}
            :people/order-eid-decending
            {:query '{:find  [[?e ...]]
                      :where [[?e :person/first-name]]}
             :sort  {:order :decending}}

            ;; Depending on other queries
            :names/any-name
            {:query  '{:find  [?name .]
                       :where [[_ :person/first-name ?name]]}
             :params {'?name [:route-params :name]}}
            :person/by-any-name
            {:query      '{:find  [?e .]
                           :where [[?e :person/first-name ?name]]}
             :depends-on [:names/any-name]
             :params     {'?name [:depends-on :names/any-name]}}})

(deftest read-test
  (let [db (-> (d/create-conn {:person/first-name {:db/unique :db.unique/identity}})
               (d/db)
               (d/db-with [{:person/first-name "Petter"}
                           {:person/first-name "Diana"}]))
        parser (parser/parser {:read (read/->read-fn reads (lime.read.datascript/db-fns))})]
    (is (= #{{:person/first-name "Petter"}
             {:person/first-name "Diana"}}
           (set
             (:people
               (parser {:db db}
                       [{:people [:person/first-name]}])))))
    (is (= {:person/first-name "Petter"}
           (:person/by-name
             (parser {:db db :route-params {:name "Petter"}}
                     [{:person/by-name [:person/first-name]}]))))

    (testing "read without query returns whatever the query returned."
      (is (= (d/entid db [:person/first-name "Petter"])
             (:person/by-name
               (parser {:db db :route-params {:name "Petter"}}
                       [:person/by-name]))))
      (is (= #{"Petter" "Diana"}
             (set
               (:people/names
                 (parser {:db db} [:people/names])))))
      (is (= ["Petter" "Diana"]
             (:people/names-decending
               (parser {:db db} [:people/names-decending])))))

    (testing "order"
      (is (= [{:person/first-name "Diana"}
              {:person/first-name "Petter"}]
             (:people
               (parser {:db db} [{:people [:person/first-name]}]))
             (:people/order-name-accending
               (parser {:db db} [{:people/order-name-accending [:person/first-name]}]))))
      (is (= [{:person/first-name "Petter"}
              {:person/first-name "Diana"}]
             (:people/order-name-decending
               (parser {:db db} [{:people/order-name-decending [:person/first-name]}]))))

      (is (apply < (:people/order-eid-accending
                     (parser {:db db} [:people/order-eid-accending]))))
      (is (apply > (:people/order-eid-decending
                     (parser {:db db} [:people/order-eid-decending])))))

    (testing ":depends-on"
      (are [name] (= name (:names/any-name
                            (parser {:db db :route-params {:name name}}
                                    [:names/any-name])))
        "Petter"
        "Diana")
      (are [name] (= {:person/first-name name}
                     (:person/by-any-name
                       (parser {:db db :route-params {:name name}}
                               [{:person/by-any-name [:person/first-name]}])))
        "Petter"
        "Diana"))))
