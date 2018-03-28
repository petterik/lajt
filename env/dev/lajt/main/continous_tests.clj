(ns lajt.main.continous-tests
  (:require [lajt.dev :as dev]
            [cljs.main :as cljs]
            [lajt.tests]))

#_(let [cljs-repl cljs.repl/repl*]
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
                        "--repl-opts" {:main 'lajt.tests}
                        "--compile-opts" {:main 'lajt.tests
                                          :watch-fn 'lajt.tests/-main}
                        "--watch" "src:test:env/dev"
                        "-c"])]
    (prn "cljs.main args: " opts)
    (apply cljs/-main opts)))
