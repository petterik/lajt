(ns lime.dev
  (:require [cljs.closure]))

(defn compilable-inputs [paths]
  (reify
    cljs.closure/Inputs
    (-paths [_]
      (mapcat cljs.closure/-paths paths))
    cljs.closure/Compilable
    (-compile [_ opts]
      (mapcat #(cljs.closure/-compile % opts)
              paths))
    (-find-sources [_ opts]
      (mapcat #(cljs.closure/-find-sources % opts)
              paths))))