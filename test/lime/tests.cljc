(ns lime.tests
  (:require
    [clojure.test]
    [lime.parser-test]
    [lime.read-test]))

(defn -main [& args]
  (clojure.test/run-all-tests #"lime.*"))
