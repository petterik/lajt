(ns lajt.main.readline
  (:require
    [rebel-readline.cljs.repl]
    [lajt.dev]
    [cljs.main :as cljs]))

(defn require-lajt-tests [repl-opts]
  (update repl-opts :repl-requires (fnil conj []) '[lajt.tests]))

;; TODO
;; Change detected, recompiling ...
;; java.lang.AssertionError: Assert failed: :nodejs target with :none
;; optimizations requires a :main entry

;; Should it be easier to watch files?
;; What about extending ISeq or PersistentVector the way I have?

;; Should using watch with node work the way I'm trying to use it?

;; I even have a main I could use?
(defn watch-source-files [repl-opts]
  (assoc repl-opts :watch
                   (lajt.dev/compilable-inputs ["src" "test" "env/dev"])))

(defn print-on-compile [repl-opts]
  (assoc repl-opts :watch-fn (fn [] (prn "BUILT"))))

(let [cljs-repl cljs.repl/repl*]
  (alter-var-root
    #'cljs.repl/repl*
    (fn [x]
      (fn [env opts]
        (prn "REPL OPTS: " opts)
        (with-redefs [cljs.repl/repl* cljs-repl]
          (rebel-readline.cljs.repl/repl*
            env
            (-> opts
                (require-lajt-tests)
                #_(watch-source-files)
                #_(print-on-compile)))))))
  nil)

(defn -main [& _]
  (let [opts (map str [
                       "--verbose" true
                       "--compile-opts" {:main 'lajt.tests
                                         :source-map true}
                       "--repl-env" "node"
                       "-r"])]
    (prn "cljs.main args: " opts)
    (apply cljs/-main opts)))
