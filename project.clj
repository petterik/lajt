(defproject org.clojars.petterik/lajt "0.1.2-SNAPSHOT"
  :repositories [["clojars" {:sign-relases false}]]
  :dependencies 
  [[org.clojure/clojure "1.10.0-alpha4" :scope "provided"]
   [org.clojure/clojurescript "1.10.217" :scope "provided"]
   [datascript "0.16.5" :scope "provided"]
   [com.stuartsierra/dependency "0.2.0"]
   [medley "1.0.0"]]

  :profiles
  {:dev {:dependencies
         [[org.clojure/test.check "0.10.0-alpha2"]
          [com.clojure-goes-fast/clj-memory-meter "0.1.0"]
          [expound "0.5.0"]]}}

  :source-paths ["src"]
  :test-paths ["test"]
)
                  
