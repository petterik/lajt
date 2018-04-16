(ns lajt.deps-test
  (:require
    [clojure.test :refer [deftest is are]]
    [lajt.read.deps :as deps]
    [com.stuartsierra.dependency :as dep]))

(def old-hard-coded-ops-order
  {:pre     [:case :depends-on :params :before]
   :actions [:query :lookup-ref :custom :no-op]
   :post    [:sort :after]})

(deftest regression-test
  (is (= deps/state-order
         (dep/topo-sort (deps/dependency-graph))))

  (let [g (-> (deps/dependency-graph)
              ;; What about our ops?
              (deps/before-pre-ops :case)
              (deps/pre-op :depends-on)
              (deps/pre-op :params)
              (deps/dependency :params :depends-on)
              (deps/right-before-action :before)
              (deps/action-op :query)
              (deps/action-op :lookup-ref)
              (deps/action-op :custom)
              (deps/action-op :no-op)
              (deps/post-op :sort)
              (deps/after-post-ops :after))]
    ;; Verifying that we can both pass the graph and the
    ;; topo-sorted graph.
    (doseq [g (list g (dep/topo-sort g))]
      (is (= (:pre old-hard-coded-ops-order)
             (deps/pre-ops g)))
      (is (= (set (:actions old-hard-coded-ops-order))
             (set (deps/action-ops g))))
      (is (= (:post old-hard-coded-ops-order)
             (deps/post-ops g)))
      (is (= (into #{} cat (vals old-hard-coded-ops-order))
             (set (deps/all-ops g)))))))