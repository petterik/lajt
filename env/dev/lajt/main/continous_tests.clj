(ns lajt.main.continous-tests
  (:require [lajt.dev :as dev]
            [cljs.main :as cljs]
            [lajt.tests]))

(let [cljs-repl cljs.repl/repl*]
  (alter-var-root
    #'cljs.repl/repl*
    (fn [_]
      (fn [env opts]
        (prn "REPL OPTS: " opts)
        (cljs-repl env (assoc opts :watch (dev/compilable-inputs
                                            ["src" "test" "env/dev"]))))))
  nil)

(defn -main [& _]
  (let [opts (mapv str ["--verbose" true
                        "--repl-env" "node"
                        "--compile-opts" {:watch-fn 'lajt.tests/-main}
                        "--watch" "test"
                        "-c"])]
    (prn "cljs.main args: " opts)
    (apply cljs/-main opts)))
