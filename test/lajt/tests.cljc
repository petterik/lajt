(ns lajt.tests
  (:require
    [clojure.test]
    [lajt.parser-test]
    [lajt.read-test]))

(defn -main [& args]
  (clojure.test/run-all-tests #"lajt.*"))
