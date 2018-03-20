(ns lime.tests
  (:require
    [clojure.test]
    [lime.parser-test]
    [lime.read-test]))

(defn -main [& args]
  (clojure.test/run-tests
    'lime.parser-test
    'lime.read-test))
