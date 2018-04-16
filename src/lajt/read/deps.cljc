(ns lajt.read.deps
  (:require
    [com.stuartsierra.dependency :as dep]))

(def state-order [:op.state/pre
                  :op.state/will-act
                  :op.state/action
                  :op.state/did-act
                  :op.state/post])

(defn- between [state1 state2]
  (fn [dep-graph op-keyword]
    (cond-> dep-graph
            (some? state1)
            (dep/depend op-keyword state1)
            (some? state2)
            (dep/depend state2 op-keyword))))

;; API for placing an op in, before or after a state.

(def before-pre-ops (between nil :op.state/pre))
(def pre-op (between :op.state/pre :op.state/will-act))
(def right-before-action (between :op.state/will-act :op.state/action))
(def action-op (between :op.state/action :op.state/did-act))
(def post-op (between :op.state/did-act :op.state/post))
(def after-post-ops (between :op.state/post nil))

(defn dependency
  "Function for depending on another dependency.
  Takes the graph, dependant and op to depend on."
  [dep-graph dependant to-depend-on]
  (dep/depend dep-graph dependant to-depend-on))

(defn- ops-between [g from to]
  (eduction
    (drop-while (if (some? from) (complement #{from}) (constantly false)))
    (take-while (if (some? to) (complement #{to}) (constantly true)))
    (remove (comp #{"op.state"} namespace))
    (cond-> g (not (seq? g)) (dep/topo-sort))))

(defn pre-ops [dep-graph]
  (ops-between dep-graph nil :op.state/action))

(defn action-ops [dep-graph]
  (ops-between dep-graph :op.state/action :op.state/did-act))

(defn post-ops [dep-graph]
  (ops-between dep-graph :op.state/did-act nil))

(defn all-ops [dep-graph]
  (ops-between dep-graph nil nil))

(defn dependency-graph []
  (reduce (fn [g [first then]]
            (dep/depend g then first))
          (dep/graph)
          (partition 2 1 state-order)))
