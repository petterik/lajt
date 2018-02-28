(ns lime.parser
  (:require [clojure.spec.alpha :as s]))

;; TODO: Support idents?
(s/def ::id keyword?)
(s/def ::read-exprs (s/coll-of ::read-expr :kind vector?))
(s/def ::join (s/map-of ::id ::read-exprs :count 1))
(s/def ::union-map (s/map-of keyword? ::read-exprs))
(s/def ::union (s/map-of ::join ::union-map :count 1))

(s/def ::param-expr (s/cat :expr (s/alt :id ::id
                                        :join ::join
                                        :union ::union)
                           :params map?))

(s/def ::read-expr (s/or
                      :id ::id
                      :join ::join
                      :union ::union
                      :param-expr ::param-expr))

(s/def ::mutation-expr
  (s/cat :id symbol?
         :params (s/? map?)))

(s/def ::query-expr (s/or :read ::read-expr
                          :mutate ::mutation-expr))

(s/def ::query (s/coll-of ::query-expr :kind vector?))

;; Lazy version of the spec used for parsing/editing/merging.
;; Why:
;; When parsing the union, there's no need to go down each path when
;; only one (or a few) of the paths is/are going to be used. Think routing.
;; There's no need to apply spec to a read's pull patterns. We can use
;; the spec for validation, but we need laziness sometimes for speed.

;; l short for lazy.
(s/def ::l-query (s/coll-of ::l-query-expr :kind vector?))
(s/def ::l-query-expr (s/or :read ::l-read-expr
                            :mutate ::mutation-expr))
(s/def ::l-read-expr (s/or
                       :id ::id
                       :join ::l-join
                       :union ::l-union
                       :param-expr ::l-param-expr))

(s/def ::l-param-expr (s/cat :expr (s/alt :id ::id
                                          :join ::l-join
                                          :union ::l-union)
                             :params map?))

(s/def ::l-read-exprs vector?)
(s/def ::l-join (s/map-of ::id ::l-read-exprs :count 1))
(s/def ::l-union-map (s/map-of keyword? vector?))
(s/def ::l-union (s/map-of keyword? ::l-union-map :count 1))

;; End lazy query spec.

(defn- dispatch [env k expr]
  (let [call-fn (get env (::type env))]
    ;; returning a pair that can be conjed into a {}.
    [k
     (call-fn (-> env
                  (assoc :dispatched-by expr))
              k
              (:params env))]))

(defmulti parse (fn [env expr] (nth expr 0)))
(defmethod parse :default
  [env expr]
  (throw (ex-info "Unknown call method." {:env env :expr expr})))

(defmethod parse :read
  [env [_ expr]]
  (parse (assoc env ::type :read) expr))

(defmethod parse :id
  [env [_ k :as expr]]
  (dispatch env k expr))

(defmethod parse :join
  [env [_ m :as expr]]
  (let [[k v] (first m)]
    (dispatch (assoc env :query v)
              k
              expr)))

(defmethod parse :union
  [env [_ m :as expr]]
  (let [[k v] (first m)]
    ;; Maybe lazily conform/unform ::unions and maybe even ::joins?
    (dispatch (assoc env :query v)
              k
              expr)))

(defmethod parse :mutate
  [env [_ {:keys [id params]} :as expr]]
  (dispatch (assoc env ::type :mutate :params params)
            id
            expr))

(defmethod parse :param-expr
  [env [_ {:keys [expr params]}]]
  (parse (assoc env :params params) expr))

(defn parser [{:keys [read mutate]}]
  (fn self [env query]
    (let [env (cond-> (assoc env :read read :mutate mutate)
                      ;; Allow the user to have a pointer to the root parser in the env.
                      (nil? (:parser env))
                      (assoc :parser self))]
      (->> (s/conform ::query query)
           ;; bubble mutations to the top.
           (sort-by (comp {:mutate 0 :read 1} first))
           (into {}
             (map (partial parse env)))))))

(defn- get-name
  "Takes a keyword or a string and returns the string, namespace or the name of it."
  [k]
  (if (string? k)
    k
    (or (namespace k)
        (name k))))

(defn lazy-parser
  "Like parser, but parses unions and joins lazily and more effectively.
  Takes:
   * :join-namespace <string or keyword>,
                     specifying a namespace to recursively
                     parse the join, not calling `read`.
   * :union-namespace <string or keyword>,
                      specifying a namespace to recursively
                      parse only a selected union(path).
   * :union-selector <4-arity fn>,
                     Passed [env k p union-map], same as `read` but
                     with an additional union-map arugment.
                     Returns a key in the union and only that
                     part of the union is parsed.

  Stuff it doesn't parse:
  - Pull patterns of reads.
  - Every branch of an union. Only the selected one(s)."
  [{:keys [read mutate join-namespace union-namespace union-selector]
    :or {union-namespace "route"
         join-namespace "child"}}]
  (fn self [env query]
    (let [read (fn [env k p]
                 (let [[dispatch-key dispatch-val] (:dispatched-by env)]
                   (cond
                     (and (= :join dispatch-key)
                          (= (get-name join-namespace)
                             (get-name k)))
                     (let [[_ join-val] (first dispatch-val)
                           conformed (s/conform (s/coll-of ::l-read-expr :kind vector?) join-val)]
                       (prn "conf: " conformed " join val: " join-val)
                       (into {}
                             (map (partial parse env))
                             conformed))

                     (and (= :union dispatch-key)
                          (= (get-name union-namespace)
                             (get-name k)))
                     (let [[_ union-val] (first dispatch-val)
                           _ (when (nil? union-selector)
                               (throw (ex-info "Dispatching on union-namespace but :union-selector was nil!"
                                               {:key           k
                                                :dispatched-by (:dispatched-by env)})))
                           selected-path (union-selector env k p (:query env))
                           selected-union (get union-val selected-path)
                           conformed-union (s/conform (s/coll-of ::l-read-expr :kind vector?)
                                                      (get union-val selected-path))]
                       (prn [selected-path selected-union conformed-union])
                       (into {}
                             (map (partial parse env))
                             conformed-union))

                     :else (read env k p))))
          env (cond-> (assoc env :read read :mutate mutate)
                      ;; Allow the user to have a pointer to the root parser in the env.
                      (nil? (:parser env))
                      (assoc :parser self))]
      (->> (s/conform ::l-query query)
           (sort-by (comp {:mutate 0 :read 1} first))
           (into {}
                 (map (partial parse env)))))))

(comment
  (def test-queries '[:read-key
                      {:join/a [:read-key]}
                      {:join/b [{:join/a [:read-key]}]}
                      {:union/a {:union.a/x [:read-key]
                                 :union.a/y [{:join/a [:read-key]}]}}
                      (:read-key2 {:param 1})
                      ({:join/c [:read-key]} {:param 1})
                      ({:union/b {:union.b/a [:read-key]}} {:param 1})
                      (mutate-no-params)
                      (mutate-with-params {:param 1})])

  (s/conform ::query test-queries)

  (s/conform ::query [:foo])
  (parse {:read prn :mutate prn} (first (s/conform ::query [:foo])))
  (parse {:read prn :mutate prn} (first (s/conform ::query [{:foo [:bar]}])))
  (parse {:read prn :mutate prn} (first (s/conform ::query [{:foo {:baz [:bar]}}])))
  (parse {:read prn :mutate prn} (first (s/conform ::query '[(:foo {:p 1})])))
  (parse {:read prn :mutate prn} (first (s/conform ::query '[(foo {:p 1})])))

  (->> test-queries
       (s/conform ::query)
       (sort-by (comp {:mutate 0 :read 1} first))
       (into {}
             (map (partial parse {:read prn :mutate prn}))))

  (->> test-queries
       (s/conform ::l-query)
       (sort-by (comp {:mutate 0 :read 1} first))
       (into {}
             (comp (map #(doto % prn))
                   (map (partial parse {:read prn :mutate prn})))))

  (s/explain ::l-query [{:join/a [:read-key]}])
  (time (dotimes [_ 1000] (s/conform ::query test-queries)))
  (time (dotimes [_ 1000] (s/conform ::l-query test-queries)))

  ((lazy-parser {:read           prn
                 :join-namespace "proxy"})
    {}
    [:read
     {:proxy/foo [{:read2 [:key]}
                  :read3]}
     {:join/read4 [:read5]}])

  ((lazy-parser {:read            prn
                 :union-namespace "union"
                 :union-selector  (fn [env k p union-map]
                                    (->> union-map
                                         keys
                                         (map name)
                                         sort
                                         last
                                         keyword))})
    {}
    [{:union/x {:a [:read1]
                :b [:read2]
                :c [:read3]}}])
  )
