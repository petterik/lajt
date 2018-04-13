(ns lajt.parser
  (:require [clojure.spec.alpha :as s]
            [medley.core :as m]))

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

(defn- query-fragment [env k expr]
  {::type      (::type env)
   ::key       k
   ::params    (:params env)
   ::query     (:query env)
   ::expr      expr
   ::expr-spec (get-in env [::expr-specs (::type env)])})

(defmulti parse (fn [env expr] (first expr)))
(defmethod parse :default
  [env expr]
  (throw (ex-info "Unknown call method." {:env env :expr expr})))

(defmethod parse :read
  [env [_ expr]]
  (parse (assoc env ::type :read) expr))

(defmethod parse :id
  [env [_ k :as expr]]
  (query-fragment (assoc env :query nil)
                  k
                  expr))

(defmethod parse :join
  [env [_ m :as expr]]
  (let [[k [expr-type v]] (first m)]
    (query-fragment (assoc env :query v)
                    k
                    expr)))

(defmethod parse :union
  [env [_ m :as expr]]
  (let [[k v] (first m)]
    ;; Maybe lazily conform/unform ::unions and maybe even ::joins?
    (query-fragment (assoc env :query v)
                    k
                    expr)))

(defmethod parse :mutate
  [env [_ {:keys [id params] :as expr}]]
  (query-fragment (assoc env ::type :mutate :params params)
                  id
                  expr))

(defmethod parse :param-expr
  [env [_ {:keys [expr params]}]]
  (parse (assoc env :params params) expr))

(def identity-plugin
  (fn
    ;; query-plugin
    ([_ query] query)
    ;; before-plugin
    ([env _ _] env)
    ;; after-plugin
    ([_ _ _ ret] ret)))

(def identity-plugin-map
  {:before identity-plugin
   :after identity-plugin})

(def unwrap-om-next-read-plugin
  {:after (fn [env k p ret]
            (if-some [t (:target env)]
              (get ret t)
              (:value ret)))})

(def unwrap-om-next-mutate-plugin
  {:after (fn [env k p ret]
            (if-some [t (:target env)]
              (get ret t)
              (when-some [a (:action ret)]
                (a))))})

;; query->parsed-query?
;; where parsed-query is:
(s/def ::type #{:read :mutate})
(s/def ::key (s/or :read keyword?
                   :mutate symbol?))
(s/def ::expr (s/or :read ::read-expr
                    :mutate ::mutation-expr))
(s/def ::expr-spec #{::mutation-expr ::read-expr ::l-read-expr})
(s/def ::params map?)
(s/def ::parsed-query-fragment (s/keys :req [::type ::key ::expr ::expr-spec]
                                       :opt [::params ::query]))
(s/def ::parsed-query (s/coll-of ::parsed-query-fragment :kind vector?))

(s/def ::query-plugin (s/fspec :args (s/cat :env map? :query ::parsed-query)
                               :ret ::parsed-query))
(s/def ::query-plugins (s/+ ::query-plugin))
(s/def ::config (s/keys :req-un [::read ::mutate]
                        :opt-un [::join-namespaces ::union-namespaces ::union-selector]))
(s/def ::env (s/keys :opt [::config ::query-plugins]))

(defn- parse-query [env query]
  (reduce (fn [query plugin]
            #_{:pre [(s/valid? ::parsed-query query)]
               :post [(s/valid? ::parsed-query %)]}
            (plugin env query))
          ((::query->parsed-query env) env query)
          (::query-plugins env)))

(s/fdef parse-query
        :args (s/cat :env ::env
                     :query ::query)
        :ret ::parsed-query)

(defn handle-read-mutate-return [{:keys [target] ::keys [expr expr-spec] :as env} k p ret]
  (if (nil? target)
    ;; Returning a pair that can be conjed into a {}.
    [k ret]
    ;; Handle reads with target
    (let [true->query #(if (true? %) (s/unform expr-spec expr) %)]
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
               :target target})))))))

(defn with-plugins-fn [read-or-mutate plugins-key]
  (fn [env k p]
    (let [plugins (get env plugins-key)
          env (transduce (keep :before)
                          (completing
                            (fn [env plugin]
                              (plugin env k (::params env))))
                          (assoc env ::params p)
                          plugins)
          val (read-or-mutate env k (::params env))]
      (transduce (keep :after)
                 (completing
                   (fn [env plugin]
                     (->> (plugin env k (::params env) (::return env))
                          (assoc env ::return)))
                   ::return)
                 (assoc env ::return val)
                 plugins))))

(defn call-reads-and-mutates-fn [read mutate]
  (let [call-fns {:read   (with-plugins-fn read ::read-plugins)
                  :mutate (with-plugins-fn mutate ::mutate-plugins)}]
    (fn [env query]
      (let [xf (map (fn [{::keys [type expr expr-spec params key query]}]
                      (let [env (assoc env ::expr expr
                                           ::expr-spec expr-spec
                                           :query query
                                           ::type type
                                           ::params params)
                            ret ((get call-fns type) env key params)]
                        (handle-read-mutate-return env key params ret))))
            query (sort-by (comp {:mutate 0 :read 1} ::type) query)]
        (if (nil? (:target env))
          (->> query
               (into {} (comp xf (remove (comp nil? second))))
               (not-empty))
          (->> query
               (into []
                     (comp xf cat (distinct)))))))))

(def eager-query-parser-plugin
  (fn [env query]
    (->> (s/conform ::query query)
         (eduction
           (map (partial parse (assoc env ::expr-specs {:read   ::read-expr
                                                        :mutate ::mutation-expr})))
           (map (fn [{::keys [query expr] :as fragment}]
                  (cond-> fragment
                          (some? query)
                          (cond->
                            (= :join (first expr))
                            (update ::query #(s/unform ::read-exprs %))
                            (= :union (first expr))
                            (update ::query #(s/unform ::union-map %))))))))))

(def lazy-query-parser-plugin
  (fn [env query]
    (eduction
      (map (partial parse (assoc env ::expr-specs {:read   ::l-read-expr
                                                   :mutate ::mutation-expr})))
      (s/conform ::l-query query))))

(defn parsed-query->query
  ([]
   (map (fn [{::keys [key query params]}]
          (cond-> key
                  (some? query)
                  (hash-map query)
                  (some? params)
                  (list params)))))
  ([parsed-query]
   (into [] (parsed-query->query) parsed-query)))

(defn parser
  "Like parser, but parses unions and joins lazily and more effectively. Stuff it doesn't parse:
  - Pull patterns of reads.
  - Every branch of a union."
  [{:keys  [read mutate read-plugins mutate-plugins query-plugins eager?]
    :as    config}]
  (let [call-read-and-mutate (call-reads-and-mutates-fn read mutate)]
    (fn self
      ([] config)
      ([env query target]
       (self (assoc env :target target) query))
      ([env query]
       (s/assert ::query query)
       (let [env (-> (assoc env ::config config
                                ::read-plugins read-plugins
                                ::mutate-plugins mutate-plugins
                                ::query-plugins query-plugins
                                ::query->parsed-query (if eager?
                                                        eager-query-parser-plugin
                                                        lazy-query-parser-plugin))
                     ;; Allow the user to have a pointer to the root parser in the env.
                     (cond-> (nil? (:parser env))
                             (assoc :parser self)))
             return (->> (parse-query env query)
                         (call-read-and-mutate env))]
         (when (:debug env)
           (locking *out*
             (prn "lajt.parser query: " query)
             (prn "lajt.parser Return: " return)
             (prn "lajt.parser target: " (:target env))))
         return)))))

(defn eager-parser
  "Conforms the whole query - including pull pattern. Takes :read and :mutate keys.

  Deprecated. Here because of never-removing-stuff rule."
  [config]
  (parser (assoc config :eager? true)))

;; Deduping query recursively - flattening query

(defn- get-name
  "Takes a keyword or a string and returns the string, namespace or the name of it."
  [k]
  (if (string? k)
    k
    (or (namespace k)
        (name k))))

(defn- name-set [names]
  (when (some? names)
    (into #{}
          (map get-name)
          (cond-> names (not (coll? names)) vector))))

(defn- flatten-query-xf [env {:keys [join-namespaces union-namespaces union-selector] :as opts}]
  (let [unions (name-set union-namespaces)
        joins (name-set join-namespaces)]
    (if (and (empty? unions)
             (empty? joins))
      (map identity)
      (mapcat (fn self [{::keys [type key expr query params] :as frag}]
                (cond
                  (= :mutate type)
                  [frag]

                  (and (= :join (first expr))
                       (contains? joins (get-name key)))
                  (eduction
                    (mapcat self)
                    ((::query->parsed-query env) env query))

                  (and (= :union (first expr))
                       (contains? unions (get-name key)))
                  (let [unions (union-selector (assoc env :query query) key params)]
                    (if (nil? unions)
                      (prn "WARN: :union-selector returned nil for key " key
                           " will leave out union from the query.")
                      (let [unions (cond-> unions (keyword? unions) vector)]
                        (if (not-any? #(contains? query %) unions)
                          (prn "WARN: queries in union found with union-selected path(s): " unions
                               " for key: " key
                               " in union-map: " query
                               ". Will leave out union from the query.")
                          (eduction
                            (keep #(get query %))
                            (mapcat #((::query->parsed-query env) env %))
                            (mapcat self)
                            unions)))))

                  :else
                  [frag]))))))

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

(defn dedupe-query-plugin
  "Flattens a query, merging pull-patterns. Will walk joins and select paths from unions.
  Takes:
  :join-namespaces <string or keyword>
  :union-namespaces <string or keyword>
  :union-selector <3-arity fn>

  The :union-selector is passed [env k p], same as `read`, and returns a single multiple paths
  in the union to be kept in the flattened query."
  [flatten-query-opts]
  (fn [env parsed-query]
    (into (filterv (comp #{:mutate} ::type) parsed-query)
          (->> parsed-query
               (eduction
                 (filter (comp #{:read} ::type))
                 (flatten-query-xf env flatten-query-opts)
                 (parsed-query->query))
               (vec)
               (merge-read-queries)
               ((::query->parsed-query env) env)))))

(defn dedupe-query
  "Takes a parser and a query, returns a new query where the reads
  have merged pull patterns.

  Old. Please use the dedupe-query-plugin if you're using lajt.parser/parser"
  [parser-or-config env query]
  (let [config (if (fn? parser-or-config)
                 (parser-or-config)
                 parser-or-config)
        env (assoc env ::query->parsed-query lazy-query-parser-plugin
                       ::query-plugins [(dedupe-query-plugin config)]
                       ::config config)]
    (->> (parse-query env query)
         (parsed-query->query))))

(comment

  )