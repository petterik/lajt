(ns repl.readline
  (:require
    [rebel-readline.cljs.repl]
    [cljs.main :as cljs]))

(defn require-lime-tests [repl-opts]
  (update repl-opts :repl-requires (fnil conj []) '[lime.tests]))

;; TODO
;; Change detected, recompiling ...
;; java.lang.AssertionError: Assert failed: :nodejs target with :none
;; optimizations requires a :main entry

;; Should it be easier to watch files?
;; What about extending ISeq or PersistentVector the way I have?

;; Should using watch with node work the way I'm trying to use it?

;; I even have a main I could use?
(defn watch-source-files [repl-opts]
  (let [paths ["src" "test" "env/dev"]]
    (assoc repl-opts :watch
                    (reify
                      cljs.closure/Inputs
                      (-paths [this]
                        (mapcat cljs.closure/-paths paths))
                      cljs.closure/Compilable
                      (-compile [this opts]
                        (mapcat #(cljs.closure/-compile % opts)
                                paths))
                      (-find-sources [this opts]
                        (mapcat #(cljs.closure/-find-sources % opts)
                                paths))))))

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
                (require-lime-tests)
                #_(watch-source-files)
                #_(print-on-compile)))))))
  nil)

(defn -main [& _]
  (let [opts (map str [
                       "--verbose" true
                       "--compile-opts" {:main 'lime.tests
                                         :source-map true}
                       "--repl-env" "node"
                       "-r"])]
    (prn "cljs.main args: " opts)
    (apply cljs/-main opts)))
