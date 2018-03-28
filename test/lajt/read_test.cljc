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

(def reads
  {:people
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
    :params     {'?name [:depends-on :names/any-name]}}

   ;; Doing something before query
   :case/before-fn
   {:before (fn [env]
              (swap! (:atom env) inc))
    :query  '{:find  [?e .]
              :where [[?e :person/first-name]]}}
   ;; Lookup ref instead of query
   :lookup/petter
   {:lookup-ref [:person/first-name "Petter"]}
   :lookup/diana
   {:lookup-ref [:person/first-name "Diana"]}
   ;; Cannot have both lookup ref and query
   :crash/lookup+query
   {:lookup-ref [:person/first-name "Petter"]
    :query      '{:find  [?e .]
                  :where [[?e :person/first-name]]}}
   })

(defn- ->db []
  (-> (d/create-conn {:person/first-name {:db/unique :db.unique/identity}})
      (d/db)
      (d/db-with [{:person/first-name "Petter"}
                  {:person/first-name "Diana"}])))

(defn- ->parser []
  (parser/parser {:read (read/->read-fn reads (lajt.read.datascript/db-fns))}))

(comment
  (do
    (def db (->db))
    (def parser
      (let [p (->parser)]
        (fn
          ([] (p))
          ([env query]
           (p (assoc env :debug true) query))))))

  (is (= #{{:person/first-name "Petter"}
           {:person/first-name "Diana"}}
         (set
           (:people
             (parser {:db db}
                     [{:people [:person/first-name]}])))))
  )

(deftest read-test
  (let [db (->db)
        parser (->parser)]
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
        "Diana"))
    :case/before-fn :lookup/petter :lookup/diana :crash/lookup+query
    (testing ":before function"
      (let [state (atom 0)]
        ;; :atom gets incremented by one in the :case/before-fn.
        (parser {:db db :atom state} [:case/before-fn])
        (is (== 1 @state))))

    (testing "reads by lookup-ref"
      (is (= (:lookup/petter (parser {:db db} [:lookup/petter]))
             (d/q '{:find [?e .]
                    :where [[?e :person/first-name "Petter"]]}
                  db)))
      (is (= (:lookup/diana (parser {:db db} [:lookup/diana]))
             (d/q '{:find [?e .]
                    :where [[?e :person/first-name "Diana"]]}
                  db)))

      (testing "can pull on lookup-refs"
        (is (= {:person/first-name "Petter"}
               (:lookup/petter (parser {:db db} [{:lookup/petter [:person/first-name]}]))))
        (is (= {:person/first-name "Diana"}
               (:lookup/diana (parser {:db db} [{:lookup/diana [:person/first-name]}])))))

      (testing "cannot have both lookup ref and query"
        (is (thrown-with-msg?
              #?(:clj Exception :cljs js/Error)
              #"lookup.*query"
              (parser {:db db} [:crash/lookup+query])))))

    (testing "Calls on key that does not exist, throws"
      (is (thrown-with-msg?
            #?(:clj Exception :cljs js/Error)
            #"[Nn]o such read"
            (parser {:db db} [:kdsoakdosa/kdsoakdsa]))))))
