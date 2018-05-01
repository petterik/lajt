(ns lajt.ops-test
  (:require
    [clojure.test :refer :all]
    [lajt.read.ops :as ops]))

(deftest defining-ops
  (let [setupf (fn [env v])
        transformf (fn [env v])
        op-args [:lajt.op.stage/setup
                 :lajt.op.stage/dependents [:foo]
                 :lajt.op.stage/depends-on [:foo]
                 :lajt.op.stage/dependents [:bar]
                 setupf
                 :lajt.op.stage/transform
                 transformf]]
    (is (= (lajt.read.ops/->op :foo op-args)
           {:lajt.op/id
            :foo
            :lajt.op/stages
            [{:lajt.op.stage/id         :lajt.op.stage/setup,
              :lajt.op.stage/depends-on [:foo],
              :lajt.op.stage/dependents [:foo :bar]
              :lajt.op.stage/fn setupf}
             {:lajt.op.stage/id         :lajt.op.stage/transform,
              :lajt.op.stage/fn transformf}]}))))


