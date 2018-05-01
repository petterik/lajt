(ns lajt.tests
  (:require
    [clojure.test]
    [lajt.parser-test]
    [lajt.read-test]
    [expound.alpha]
    [clojure.spec.alpha :as s]
    [clojure.spec.test.alpha :as st]))

(defn -main [& args]
  (clojure.test/run-all-tests #"lajt.*"))

(s/check-asserts true)
(set! s/*explain-out* expound.alpha/printer)

(st/instrument)

(comment
  (-main))
