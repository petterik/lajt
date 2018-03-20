(ns repl.readline
  (:require
    [rebel-readline.cljs.repl]
    [cljs.main :as cljs]))

(let [cljs-repl cljs.repl/repl*]
  (alter-var-root
    #'cljs.repl/repl*
    (fn [x]
      (fn [env opts]
        (with-redefs [cljs.repl/repl* cljs-repl]
          (rebel-readline.cljs.repl/repl* env opts)))))
  nil)

(defn -main [& args]
  (let [opts (map str [
                       ;;"--verbose" true
                       "--compile-opts" {:source-map true}])
        args (vec (concat opts args))]
    (prn "cljs.main args: " args)
    (apply cljs/-main args)))
