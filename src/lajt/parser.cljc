(ns lajt.parser
  (:require [clojure.spec.alpha :as s]))

;; TODO: Support idents?
(s/def ::id keyword?)
(s/def ::read-exprs (s/coll-of ::read-expr :kind vector? :gen-max 3))
(s/def ::recursion (s/or :depth nat-int? :unbounded #{'...}))
(s/def ::join-val (s/or :read-exprs ::read-exprs :recursion ::recursion))
(s/def ::join (s/map-of ::id ::join-val :count 1 :gen-max 3))
(s/def ::union-map (s/map-of keyword? ::read-exprs :gen-max 3))
(s/def ::union (s/map-of ::id ::union-map :count 1 :gen-max 3))

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
(s/def ::l-join-val (s/or :read-exprs ::l-read-exprs
                          :recursion ::recursion))
(s/def ::l-join (s/map-of ::id ::l-join-val :count 1))
(s/def ::l-union-map (s/map-of keyword? ::l-read-exprs))
(s/def ::l-union (s/map-of ::id ::l-union-map :count 1))

;; End lazy query spec.

(defn- handle-target-return [{:keys [target dispatched-by] :as env} k ret]
  (let [true->query (fn [x]
                      (if (true? x)
                        (let [unform-key (get-in env [:unform-keys (::type env)])]
                          (s/unform unform-key dispatched-by))
                        x))]
    (when (some? ret)
     (cond
       (true? ret) [(true->query ret)]
       (sequential? ret) (into [] (map true->query) ret)
       :else
       (throw
         (ex-info
           (str "Unable to handle return from k: " k
                " with :target " target
                " return: " ret)
           {:k      k
            :ret    ret
            :target target}))))))

(defn- dispatch [env k expr]
  (let [call-fn (get env (::type env))
        env (assoc env :dispatched-by expr)
        ret (call-fn env k (:params env))]
    (if (nil? (:target env))
      ;; Returning a pair that can be conjed into a {}.
      [k ret]
      ;; Handle reads with target
      (handle-target-return env k ret))))

(defmulti parse (fn [env expr] (first expr)))
(defmethod parse :default
  [env expr]
  (throw (ex-info "Unknown call method." {:env env :expr expr})))

(defmethod parse :read
  [env [_ expr]]
  (parse (assoc env ::type :read) expr))

(defmethod parse :id
  [env [_ k :as expr]]
  (dispatch (assoc env :query nil)
            k
            expr))

(defmethod parse :join
  [env [_ m :as expr]]
  (let [[k [expr-type v]] (first m)]
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
  [env [_ {:keys [id params] :as expr}]]
  (dispatch (assoc env ::type :mutate :params params)
            id
            expr))

(defmethod parse :param-expr
  [env [_ {:keys [expr params]}]]
  (parse (assoc env :params params) expr))

(defn assert-spec [spec x]
  (if (s/valid? spec x)
    x
    (let [ed (s/explain-data spec x)]
      (throw (ex-info
               (str "Spec assertion failed\n" (with-out-str (s/explain-out ed)))
               ed)))))

(defn- join? [env]
  (vector? (:query env)))

(defn- union? [env]
  (map? (:query env)))

(defn- unwrap-om-next-read [read]
  (fn [env k p]
    (let [ret (read env k p)]
      (if-some [t (:target env)]
        (get ret t)
        (:value ret)))))

(defn- unwrap-om-next-mutate [mutate]
  (fn [env k p]
    (let [ret (mutate env k p)]
      (if-some [t (:target env)]
        (get ret t)
        (when-some [a (:action ret)]
          (a))))))

(defn eager-parser
  "Conforms the whole query - including pull pattern. Takes :read and :mutate keys."
  [{:keys [read mutate om-next?]}]
  (fn self [env query]
    (assert-spec ::query query)
    (let [read (fn [{:keys [query] :as env} k p]
                 ;; Since this parser conforms the whole query at once, we want to
                 ;; unform the :query before calling read.
                 (let [env (cond-> env
                                   (join? env)
                                   (update :query #(s/unform ::read-exprs %))
                                   (union? env)
                                   (update :query #(s/unform ::union-map %)))]
                   (read env k p)))
          env (cond-> (assoc env :read (cond-> read om-next? (unwrap-om-next-read))
                                 :mutate (cond-> mutate om-next? (unwrap-om-next-mutate))
                                 :unform-keys {:read   ::read-expr
                                               :mutate ::mutation-expr})
                      ;; Allow the user to have a pointer to the root parser in the env.
                      (nil? (:parser env))
                      (assoc :parser self))
          ret (->> (s/conform ::query query)
                   ;; bubble mutations to the top.
                   (sort-by (comp {:mutate 0 :read 1} first))
                   (map (partial parse env)))]
      (if (nil? (:target env))
        (not-empty (into {} (remove (comp nil? second)) ret))
        (into [] (comp cat (distinct)) ret)))))

(defn- get-name
  "Takes a keyword or a string and returns the string, namespace or the name of it."
  [k]
  (if (string? k)
    k
    (or (namespace k)
        (name k))))

(defn parser
  "Like parser, but parses unions and joins lazily and more effectively.
  Takes:
   * :join-namespace <string or keyword>,
                     specifying a namespace to recursively
                     parse the join, not calling `read`.
   * :union-namespace <string or keyword>,
                      specifying a namespace to recursively
                      parse only a selected union(path).
   * :union-selector <3-arity fn>,
                     Passed [env k p], same as `read`.
                     Returns a key in the union and only that
                     part of the union is parsed.

  Stuff it doesn't parse:
  - Pull patterns of reads.
  - Every branch of an union. Only the selected one(s)."
  [{:keys [read mutate join-namespace union-namespace union-selector om-next?]
    :as config}]
  (fn self
    ([] config)
    ([env query target]
     (self (assoc env :target target) query))
    ([env query]
     (s/assert ::query query)
     (let [read (fn [env k p]
                  (let [[dispatch-key dispatch-val] (:dispatched-by env)]
                    (cond
                      (and (= :join dispatch-key)
                           (some? join-namespace)
                           (= (get-name join-namespace)
                              (get-name k)))
                      (let [[_ [_ join-val]] (first dispatch-val)
                            conformed (s/conform (s/coll-of ::l-read-expr :kind vector?) join-val)]
                        (into {}
                              (map (partial parse (dissoc env :query)))
                              conformed))

                      (and (= :union dispatch-key)
                           (some? union-namespace)
                           (= (get-name union-namespace)
                              (get-name k)))
                      (let [[_ union-val] (first dispatch-val)
                            _ (when (nil? union-selector)
                                (throw (ex-info "Dispatching on union-namespace but :union-selector was nil!"
                                                {:key           k
                                                 :dispatched-by (:dispatched-by env)})))
                            selected-path (union-selector
                                            ;; Assoc the initial read function (not this one)
                                            ;; Such that the union selector can dispatch
                                            ;; to the read function.
                                            (assoc env :read read
                                                       ::calling-union-selector true)
                                            k
                                            p)
                            conformed-union (s/conform (s/coll-of ::l-read-expr :kind vector?)
                                                       (get union-val selected-path))]
                        (into {}
                              (map (partial parse (dissoc env :query)))
                              conformed-union))

                      :else (read env k p))))

           env (cond-> (assoc env :read (cond-> read om-next? (unwrap-om-next-read))
                                  :mutate (cond-> mutate om-next? (unwrap-om-next-mutate))
                                  :unform-keys {:read   ::l-read-expr
                                                :mutate ::mutation-expr})
                       ;; Allow the user to have a pointer to the root parser in the env.
                       (nil? (:parser env))
                       (assoc :parser self))
           ret (->> (s/conform ::l-query query)
                    (sort-by (comp {:mutate 0 :read 1} first))
                    (map (partial parse env)))
           return (if (nil? (:target env))
                    (not-empty (into {} (remove (comp nil? second)) ret))
                    (into [] (comp cat (distinct)) ret))]
       (when (:debug env)
         (locking *out*
           (prn "lajt.parser query: " query)
           (prn "lajt.parser Return (before xform): " (vec ret))
           (prn "lajt.parser Return (after xform): " return)
           (prn "lajt.parser target: " (:target env))))
       return))))


;; Merging queries

(def largest-number #?(:clj Long/MAX_VALUE :cljs js/Number.MAX_SAFE_INTEGER))

(defn- order-keeping-map
  ([] {})
  ([ms]
   (let [key-order (into {}
                         (comp (mapcat keys)
                               (distinct)
                               (map-indexed #(vector %2 %1)))
                         ms)]
     (sorted-map-by #(compare (get key-order %1 largest-number)
                              (get key-order %2 largest-number))))))

(defn merge-ordered-with [f & ms]
  (let [merge-entry (fn [m e]
                      (let [k (key e) v (val e)]
                        (if (contains? m k)
                          (assoc m k (f (get m k) v))
                          (assoc m k v))))
        merge2 (fn [m1 m2]
                 (reduce merge-entry m1 (seq m2)))]
    (reduce merge2 (order-keeping-map ms) ms)))

(defn- merge-ordered [& ms]
  (into (order-keeping-map ms) cat ms))

(defn- assoc-ordered [m k v]
  (merge-ordered m {k v}))

(defmulti ->pattern-map (fn [env expr] (first expr)))
(defmethod ->pattern-map :read
  [m [_ expr]]
  (->pattern-map m expr))

(defmethod ->pattern-map :join
  [{::keys [params] :as m} [_ join]]
  (let [[k [expr-type v]] (first join)]
    ;; Updates the key which is a pair of key+params.
    (merge-ordered-with #(merge-ordered-with merge %1 %2)
                        (dissoc m ::params)
                        {(cond-> k (some? params) (vector params))
                         (reduce ->pattern-map (order-keeping-map) v)})))

(defmethod ->pattern-map :id
  [{::keys [params] :as m} [_ v]]
  (let [key (cond-> v (some? params) (vector params))]
    (-> m
        (dissoc ::params)
        (assoc-ordered key nil))))

(defmethod ->pattern-map :param-expr
  [m [_ {:keys [expr params]}]]
  (->pattern-map (assoc m ::params params) expr))

(defmethod ->pattern-map :union
  [m expr]
  (throw (ex-info (str "Unions are not allowed when merging pull patterns."
                       " Resolve all unions in the query before passing it"
                       " to this function")
                  {:map-so-far m :expr expr})))

(defn query->pattern-map [query]
  (reduce ->pattern-map {} (s/conform ::query query)))

(comment
  (query->pattern-map '[(:foo {:a 1})])
  (query->pattern-map [{:read-key [:a]}])
  (query->pattern-map [:a :b :c]))

(defn pattern-map->query [pattern-map]
  (letfn [(kv->pattern-item [[id v]]
            (let [[k params] (cond-> id (keyword? id) vector)]
              (cond-> (if (map? v)
                        {k (mapv kv->pattern-item v)}
                        k)
                      (some? params)
                      (list params))))]
    (into []
          (map kv->pattern-item)
          pattern-map)))

(defn merge-read-queries
  "Takes a query (without unions), where all the joins corresponds to a `read-root` + a pattern,
  merges all the patterns for all read-roots and returns a new query."
  [query]
  (-> (query->pattern-map query)
      (pattern-map->query)))

;; Deduping query recursively
(defn dedupe-query
  "Takes a parser and a query, returns a new query where the reads
  have merged pull patterns."
  [parser-or-config env query]
  (let [reads (atom [])
        mutates (atom [])
        config (if (fn? parser-or-config)
                 (parser-or-config)
                 parser-or-config)
        parser (parser
                 (assoc config
                   :read (fn [env k params]
                           (if-some [r (when (::calling-union-selector env)
                                         (:read config))]
                             ;; If we're calling union-selector, add the original
                             ;; read back into the env, as the union selector can
                             ;; call whatever it wants.
                             (r (assoc env :read r) k params)
                             (swap! reads conj
                                    (cond-> (s/unform ::l-read-expr (:dispatched-by env))
                                            (some? params)
                                            (list params)))))
                   :mutate (fn [env _ _]
                             (swap! mutates conj (s/unform ::mutation-expr (:dispatched-by env))))))]
    (parser env query)
    (into @mutates (merge-read-queries @reads))))

(comment

  )