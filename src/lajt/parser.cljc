(ns lajt.parser
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [medley.core :as m]))

(defn ?spec-throw [spec x]
  (when-not (s/valid? spec x)
    (throw (ex-info (str "Spec failed:\n" (s/explain-str spec x))
                    (s/explain-data spec x)))))

;; TODO: Support idents?
(s/def ::id keyword?)
(s/def ::read-exprs (s/coll-of ::read-expr :kind vector? :gen-max 2))
(s/def ::recursion (s/or :depth nat-int? :unbounded #{'...}))
(s/def ::join-val (s/or :read-exprs ::read-exprs :recursion ::recursion))
(s/def ::join (s/map-of ::id ::join-val :count 1 :gen-max 3))
(s/def ::union-map (s/map-of keyword? ::read-exprs :gen-max 1))
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

(s/def ::read-id ::id)
(s/def ::mutation-id symbol?)

(s/def ::mutation-expr
  (s/cat :id ::mutation-id
         :params (s/? map?)))

(s/def ::query-expr (s/or :read ::read-expr
                          :mutate ::mutation-expr))

(s/def ::query (s/nilable (s/coll-of ::query-expr :kind vector? :gen-max 3)))


;; Lazy version of the spec used for parsing/editing/merging.
;; Why:
;; When parsing the union, there's no need to go down each path when
;; only one (or a few) of the paths is/are going to be used. Think routing.
;; There's no need to apply spec to a read's pull patterns. We can use
;; the spec for validation, but we need laziness sometimes for speed.

;; l short for lazy.
(s/def ::l-query (s/coll-of ::l-query-expr :kind vector? :gen-max 3))
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
   ::recursion (:recursion env)
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
    (condp = expr-type
      :read-exprs
      (query-fragment (assoc env :query v) k expr)
      :recursion
      (query-fragment (assoc env :recursion (s/unform ::recursion v))
                      k
                      expr))

    ))

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

(defmethod parse :recursion
  [env [_ expr]]
  )

(defn lazy-query-parser-plugin
  ([env query]
   (?spec-throw ::l-query query)
   (into []
         (map (partial parse (assoc env ::expr-specs {:read   ::l-read-expr
                                                      :mutate ::mutation-expr})))
         (s/conform ::l-query query))))

(defn query->parsed-query
  ([query]
   (query->parsed-query {} query))
  ([env query]
   (lazy-query-parser-plugin env query)))

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
(s/def ::expr any?)
(s/def ::expr-spec #{::mutation-expr ::read-expr ::l-read-expr})
(s/def ::params (s/nilable map?))

(s/def ::parsed-query-fragment
  (s/keys :req [::type ::key ::expr ::expr-spec]
          :opt [::params ::query]))

(declare parsed-query->query)

(s/def ::parsed-query
  (s/with-gen
    (s/coll-of ::parsed-query-fragment :gen-max 3)
    #(gen/fmap
      (comp (partial query->parsed-query {}) vec)
      (s/gen ::query))))

(comment
  (gen/sample (s/gen ::query))
  (def queries *1)
  (def queries (filter seq queries))
  (->> queries (map (partial query->parsed-query {})))
  (gen/sample (s/gen ::parsed-query))
  (gen/sample (gen/fmap (fn [pq] (query->parsed-query {} (parsed-query->query pq)))
                        (s/gen (s/coll-of ::parsed-query-fragment :gen-max 3)))))

(s/def ::query-plugin (s/fspec :args (s/cat :env (s/keys :req [::query->parsed-query])
                                            :parsed-query ::parsed-query)
                               :ret ::parsed-query))
(s/def ::query-plugins (s/nilable
                         (s/coll-of ::query-plugin)))

(s/def ::query->parsed-query
  (s/with-gen ifn? #(s/gen #{query->parsed-query})))

(comment
  (require '[clojure.spec.gen.alpha :as gen])
  (map (juxt identity
             (partial s/conform ::expr))
       (gen/sample (s/gen ::expr)))
  )

(s/def ::join-namespaces (s/* keyword?))
(s/def ::union-namespaces (s/* keyword?))
(s/def ::union-selector-ret (s/nilable
                              (s/or :key ::key
                                    :keys (s/coll-of ::key))))
(s/def ::union-selector (s/fspec :args (s/cat :env map? :key ::key :params ::params)
                                 :ret ::union-selector-ret))

(s/def ::config (s/keys :opt-un [::join-namespaces ::union-namespaces ::union-selector]))
(s/def ::env (s/keys :opt [::query->parsed-query ::config ::query-plugins]))

(defn- parse-query [env query]
  (reduce (fn [query plugin]
            #_{:pre [(s/valid? ::parsed-query query)]
               :post [(s/valid? ::parsed-query %)]}
            (plugin env query))
          ((::query->parsed-query env) env query)
          (::query-plugins env)))

#_(s/fdef parse-query
        :args (s/cat :env ::env
                     :query ::query)
        :ret ::parsed-query)

(defn handle-read-mutate-return [{:keys [target] ::keys [expr expr-spec] :as env} k p ret]
  (if (nil? target)
    ;; Returning a pair that can be conjed into a {}.
    [k ret]
    ;; Handle reads with target
    (let [true->query #(cond-> (if (true? %) (s/unform expr-spec expr) %)
                               (and (= :read (::type env)) p)
                               (list p))]
      (when ret
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
      (let [xf (map (fn [{::keys [type expr expr-spec
                                  params key query recursion]}]
                      (let [env (assoc env ::expr expr
                                           ::expr-spec expr-spec
                                           :query (or query recursion)
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

(defn parsed-query->query
  ([]
   (map (fn [{::keys [type key query params recursion]}]
          (condp = type
            :read
            (cond-> key
                    (some? query)
                    (hash-map query)
                    (some? recursion)
                    (hash-map recursion)
                    (some? params)
                    (list params))
            :mutate
            (if (some? params)
              (list key params)
              (list key))))))
  ([parsed-query]
   (into [] (parsed-query->query) parsed-query)))

(declare merge-read-queries)
(defn merge-queries [query & queries]
  (if-let [queries (seq (filter seq queries))]
    (merge-read-queries (into query cat queries))
    query))

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
                                ::merge-queries merge-queries
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

(def query-fragment->query-root (juxt ::key ::params))

(defn- query-roots [parsed-query]
  (when (seq parsed-query)
    (eduction
      (comp (map query-fragment->query-root)
            (distinct))
      parsed-query)))

(defn normalize-query2
  ([query] (normalize-query2 {} (query->parsed-query query)))
  ([norm parsed-query]
   (if (empty? parsed-query)
     norm
     (reduce
       (fn [norm {::keys [key params query]}]
         (let [norm-key [key params]
               union? (map? query)]
           (if union?
             (let [norm (reduce-kv
                          (fn [norm _ query]
                            (let [parsed-query (query->parsed-query query)]
                              (normalize-query2 norm parsed-query)))
                          norm
                          query)]
               (update norm
                       norm-key
                       (fnil update {:key    key
                                     :params params
                                     :union? union?})
                       :unions
                       (fnil
                         (fn [unions]
                          (reduce-kv
                            (fn [unions union query]
                              (update unions union
                                      (fnil into [])
                                      (remove (set (get unions union)))
                                      (query-roots
                                        (query->parsed-query query))))
                            unions
                            query))
                         {})))
             (let [parsed-query (when (seq query) (query->parsed-query query))
                   norm (normalize-query2 norm parsed-query)]
               (update norm
                       norm-key
                       (fnil update {:key    key
                                     :params params
                                     :union? union?})
                       :children
                       (fnil into [])
                       (remove (set (get-in norm [norm-key :children])))
                       (query-roots parsed-query))))))
       norm
       parsed-query))))

(defn denormalize-query
  ([normalized-query]
   (fn root->query
     ([root] (root->query #{} root))
     ([visited root]
      (let [root->query (partial root->query (conj visited root))
            {:keys [key params children union? unions]}
            (get normalized-query root)]
        (if union?
          (cond-> {key (m/map-vals #(mapv root->query %) unions)}
                  (some? params)
                  (list params))
          (cond-> key
                  (and (seq children)
                       ;; Avoid infinite recursion
                       (not (contains? visited root)))
                  (hash-map (mapv root->query children))
                  (some? params)
                  (list params)))))))
  ([query normalized-query]
   (into []
         (map (denormalize-query normalized-query))
         (query-roots (query->parsed-query query)))))

(defn merge-read-queries
  "Takes a query and merges all the patterns for all read-roots and returns a new query."
  [query]
  (let [parsed-query (query->parsed-query query)
        parsed-query-by-root (group-by query-fragment->query-root
                                       parsed-query)]
    (into []
          (map (fn [root]
                 (let [parsed-query (get parsed-query-by-root root)]
                   (let [norm (normalize-query2 {} parsed-query)]
                     ((denormalize-query norm) root)))))
          (query-roots parsed-query))))

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
