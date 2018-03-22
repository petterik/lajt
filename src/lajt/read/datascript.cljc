(ns lajt.read.datascript
  (:require
    [datascript.core :as d]))

(defn db-fns []
  {:q         d/q
   :entity    d/entity
   :pull      d/pull
   :pull-many d/pull-many})
