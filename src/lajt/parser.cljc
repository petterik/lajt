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

(defn- dispatch [env k expr]
  (let [call-fn (get env (::type env))]
    ;; returning a pair that can be conjed into a {}.
    [k
     (call-fn (-> env
                  (assoc :dispatched-by expr))
              k
              (:params env))]))

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

(defn- targeted-return [ret]
  (if (some? ret)
    ;; TODO: Handle remote queries.
    ret
    []))

(defn eager-parser
  "Conforms the whole query - including pull pattern. Takes :read and :mutate keys."
  [{:keys [read mutate]}]
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
          env (cond-> (assoc env :read read
                                 :mutate mutate)
                      ;; Allow the user to have a pointer to the root parser in the env.
                      (nil? (:parser env))
                      (assoc :parser self))
          ret (->> (s/conform ::query query)
               ;; bubble mutations to the top.
               (sort-by (comp {:mutate 0 :read 1} first))
               (into {}
                     (map (partial parse env)))
               (not-empty))]
      (cond-> ret
              (some? (:target env))
              (targeted-return)))))

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
  [{:keys [read mutate join-namespace union-namespace union-selector]
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
           env (cond-> (assoc env :read read :mutate mutate)
                       ;; Allow the user to have a pointer to the root parser in the env.
                       (nil? (:parser env))
                       (assoc :parser self))
           ret (->> (s/conform ::l-query query)
                    (sort-by (comp {:mutate 0 :read 1} first))
                    (into {}
                          (map (partial parse env)))
                    (not-empty))]
       (cond-> ret
               (some? (:target env))
               (targeted-return))))))

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
  (def bad-query '[:query/current-route #:proxy{:navbar [#:query{:cart [#:user.cart{:items [:db/id #:store.item{:_skus [#:store{:_items [#:store{:status [:status/type]}]}]}]} #:user{:_cart [:db/id]}]} #:query{:auth [:db/id :user/email #:user{:stripe [:stripe/id]} #:user{:profile [:user.profile/name #:user.profile{:photo [:photo/path :photo/id]}]}]} #:query{:owned-store [:db/id :store/username #:store{:locality [:sulo-locality/path]} #:store{:profile [:store.profile/name #:store.profile{:photo [:photo/path]}]} #:store{:owners [#:store.owner{:user [:db/id]}]}]} #:query{:navigation [:category/name :category/label :category/path :category/route-map]} #:proxy{:loading-bar [#:query{:loading-bar [:ui.singleton.loading-bar/show?]}]} #:proxy{:login-modal [#:proxy{:login [:query/current-route :query/messages]} #:query{:login-modal [:ui.singleton.login-modal/show?]}]} #:proxy{:notification [:query/current-route :query/firebase #:query{:auth [:db/id]} #:query{:owned-store [:db/id :store/username]} :query/notification-count #:query{:notifications [:user.notification/id :user.notification/value]}]} :query/current-route]} #:proxy{:sidebar [#:query{:cart [#:user.cart{:items [:db/id #:store.item{:_skus [#:store{:_items [#:store{:status [:status/type]}]}]}]} #:user{:_cart [:db/id]}]} #:query{:auth [:db/id :user/email #:user{:stripe [:stripe/id]} #:user{:profile [:user.profile/name #:user.profile{:photo [:photo/path :photo/id]}]}]} #:query{:owned-store [:db/id :store/username #:store{:locality [:sulo-locality/path]} #:store{:profile [:store.profile/name #:store.profile{:photo [:photo/path]}]} #:store{:owners [#:store.owner{:user [:db/id]}]}]} #:query{:navigation [:category/name :category/label :category/path :category/route-map]} #:proxy{:loading-bar [#:query{:loading-bar [:ui.singleton.loading-bar/show?]}]} #:proxy{:login-modal [#:proxy{:login [:query/current-route :query/messages]} #:query{:login-modal [:ui.singleton.login-modal/show?]}]} #:proxy{:notification [:query/current-route :query/firebase #:query{:auth [:db/id]} #:query{:owned-store [:db/id :store/username]} :query/notification-count #:query{:notifications [:user.notification/id :user.notification/value]}]} :query/current-route]} #:proxy{:footer [#:query{:sulo-localities [:db/id :sulo-locality/path :sulo-locality/title #:sulo-locality{:photo [:photo/id]}]} #:query{:auth [:db/id]} :query/current-route]} #:routing{:app-root {:unauthorized [#:query{:featured-items [:db/id :store.item/name :store.item/price :store.item/index :store.item/not-found? #:store.item{:photos [#:store.item.photo{:photo [:photo/path :photo/id]} :store.item.photo/index]} #:store.item{:skus [:db/id :store.item.sku/variation :store.item.sku/inventory]} :store.item/description :store.item/section #:store.item{:category [:category/label :category/path :category/name]} #:store{:_items [#:store{:profile [#:store.profile{:photo [:photo/path :photo/id]} :store.profile/name :store.profile/tagline]} #:store{:owners [#:store.owner{:user [:user/online?]}]} #:stream{:_store [:stream/state]} #:store{:locality [:sulo-locality/path]} #:store{:shipping [#:shipping{:rules [#:shipping.rule{:destinations [:country/code]}]}]} #:store{:status [:status/type]} :store/username]}]} :query/current-route :query/messages], :user-settings [#:query{:auth [:user/email #:user{:profile [:user.profile/name #:user.profile{:photo [:photo/id]}]} :user/stripe]} #:query{:stripe-customer [:db/id :stripe/sources :stripe/default-source :stripe/shipping]} #:query{:countries [:country/name :country/code]} :query/auth0-info :query/current-route :query/messages], :index [:query/current-route #:query{:featured-items [:db/id :store.item/name :store.item/price :store.item/index :store.item/not-found? #:store.item{:photos [#:store.item.photo{:photo [:photo/path :photo/id]} :store.item.photo/index]} #:store.item{:skus [:db/id :store.item.sku/variation :store.item.sku/inventory]} :store.item/description :store.item/section #:store.item{:category [:category/label :category/path :category/name]} #:store{:_items [#:store{:profile [#:store.profile{:photo [:photo/path :photo/id]} :store.profile/name :store.profile/tagline]} #:store{:owners [#:store.owner{:user [:user/online?]}]} #:stream{:_store [:stream/state]} #:store{:locality [:sulo-locality/path]} #:store{:shipping [#:shipping{:rules [#:shipping.rule{:destinations [:country/code]}]}]} #:store{:status [:status/type]} :store/username]}]} #:query{:featured-vods [:vod/timestamp #:vod{:store [:db/id :store/username #:store{:profile [:store.profile/name #:store.profile{:photo [:photo/path :photo/id]}]}]}]} #:query{:featured-stores [:db/id #:store{:profile [:store.profile/name :store.profile/tagline #:store.profile{:photo [:photo/path :photo/id]}]} #:store{:locality [:sulo-locality/path]} #:store{:status [:status/type]} #:stream{:_store [:stream/state]} :store/visitor-count :store/username :store/created-at :store/featured #:store{:owners [#:store.owner{:user [:user/online?]}]}]} #:query{:featured-streams [:db/id :stream/title :stream/state #:stream{:store [:db/id #:store{:locality [:sulo-locality/path]} #:store{:profile [:store.profile/name #:store.profile{:photo [:photo/path :photo/id]}]} #:store{:status [:status/type]} :store/visitor-count :store/username]}]} #:query{:auth [:db/id :user/email]} #:query{:owned-store [:db/id #:store{:locality [:sulo-locality/path]} #:store{:status [:status/type]} #:store{:profile [:store.profile/name #:store.profile{:photo [:photo/path]}]} #:store{:owners [#:store.owner{:user [:db/id]}]}]}], :store [#:proxy{:chat [#:query{:chat [:chat/store :chat/modes #:chat{:messages [:chat.message/client-side-message? :chat.message/id #:chat.message{:user [:user/email :db/id #:user{:profile [#:user.profile{:photo [:photo/id]} :user.profile/name]}]} :chat.message/text :chat.message/created-at]}]}]} #:query{:store [:db/id #:store{:locality [:sulo-locality/path]} #:store{:sections [:store.section/label :store.section/path :db/id]} :store/visitor-count :store/username #:store{:geolocation [:geolocation/title]} :store/not-found? #:store{:status [:status/type]} #:stream{:_store [:stream/state :stream/title]} #:store{:profile [:store.profile/name :store.profile/description :store.profile/tagline :store.profile/return-policy #:store.profile{:photo [:photo/path :photo/id]} #:store.profile{:cover [:photo/path :photo/id]}]} #:store{:owners [#:store.owner{:user [:user/online?]}]} #:store{:shipping [:shipping/policy]}]} #:query{:featured-items [:db/id :store.item/name :store.item/price :store.item/created-at #:store.item{:photos [#:store.item.photo{:photo [:photo/path :photo/id]} :store.item.photo/index]} #:store{:_items [#:store{:profile [:store.profile/name]}]}]} #:query{:featured-stores [:db/id #:store{:profile [:store.profile/name #:store.profile{:photo [:photo/path :photo/id]}]} :store/created-at :store/featured #:store{:items [:db/id #:store.item{:photos [#:store.item.photo{:photo [:photo/path :photo/id]} :store.item.photo/index]}]}]} #:query{:store-vods [:vod/timestamp #:vod{:store [:db/id :store/username #:store{:profile [:store.profile/name #:store.profile{:photo [:photo/path :photo/id]}]}]}]} #:query{:store-items [:db/id :store.item/name :store.item/price :store.item/index :store.item/not-found? #:store.item{:photos [#:store.item.photo{:photo [:photo/path :photo/id]} :store.item.photo/index]} #:store.item{:skus [:db/id :store.item.sku/variation :store.item.sku/inventory]} :store.item/description :store.item/section #:store.item{:category [:category/label :category/path :category/name]} #:store{:_items [#:store{:profile [#:store.profile{:photo [:photo/path :photo/id]} :store.profile/name :store.profile/tagline]} #:store{:owners [#:store.owner{:user [:user/online?]}]} #:stream{:_store [:stream/state]} #:store{:locality [:sulo-locality/path]} #:store{:shipping [#:shipping{:rules [#:shipping.rule{:destinations [:country/code]}]}]} #:store{:status [:status/type]} :store/username]}]} :query/current-route], :checkout [#:query{:checkout [:db/id :store.item.sku/variation :store.item.sku/inventory #:store.item{:_skus [:store.item/name :store.item/price #:store.item{:photos [#:store.item.photo{:photo [:photo/id]} :store.item.photo/index]} #:store{:_items [:db/id #:store{:shipping [#:shipping{:rules [#:shipping.rule{:destinations [:country/code]} #:shipping.rule{:rates [:shipping.rate/first :shipping.rate/additional :shipping.rate/free-above :shipping.rate/title]}]}]} #:store{:profile [:store.profile/name #:store.profile{:photo [:photo/id]}]} #:store{:status [:status/type]}]}]}]} #:query{:stripe-customer [:stripe/id :stripe/sources :stripe/shipping :stripe/default-source]} #:query{:auth [:db/id :user/email :user/stripe]} #:query{:countries [:country/code :country/name]} #:query{:taxes [:taxes/id :taxes/rate :taxes/freight-taxable?]} :query/messages], :browse [#:query{:browse-products-2 [#:browse-result{:items [:db/id :store.item/name :store.item/price :store.item/index :store.item/not-found? #:store.item{:photos [#:store.item.photo{:photo [:photo/path :photo/id]} :store.item.photo/index]} #:store.item{:skus [:db/id :store.item.sku/variation :store.item.sku/inventory]} :store.item/description :store.item/section #:store.item{:category [:category/label :category/path :category/name]} #:store{:_items [#:store{:profile [#:store.profile{:photo [:photo/path :photo/id]} :store.profile/name :store.profile/tagline]} #:store{:owners [#:store.owner{:user [:user/online?]}]} #:stream{:_store [:stream/state]} #:store{:locality [:sulo-locality/path]} #:store{:shipping [#:shipping{:rules [#:shipping.rule{:destinations [:country/code]}]}]} #:store{:status [:status/type]} :store/username]}]} :browse-result/meta]} #:query{:navigation [:db/id :category/name :category/label :category/path :category/route-map #:category{:children ...}]} #:proxy{:product-filters [#:query{:navigation [:category/name :category/label :category/path :category/route-map]} :query/current-route]} :query/current-route], :tos [:query/current-route #:query{:auth [:db/id]}], :shopping-bag [#:query{:cart [#:user{:_cart [:db/id]} #:user.cart{:items [:store.item.sku/variation :db/id :store.item.sku/inventory #:store.item{:_skus [:store.item/price #:store.item{:photos [#:store.item.photo{:photo [:photo/id]} :store.item.photo/index]} :store.item/name #:store{:_items [:db/id #:store{:status [:status/type]} #:store{:profile [:store.profile/name #:store.profile{:photo [:photo/id]}]}]}]}]}]} #:query{:featured-items [:db/id :store.item/name :store.item/price :store.item/index :store.item/not-found? #:store.item{:photos [#:store.item.photo{:photo [:photo/path :photo/id]} :store.item.photo/index]} #:store.item{:skus [:db/id :store.item.sku/variation :store.item.sku/inventory]} :store.item/description :store.item/section #:store.item{:category [:category/label :category/path :category/name]} #:store{:_items [#:store{:profile [#:store.profile{:photo [:photo/path :photo/id]} :store.profile/name :store.profile/tagline]} #:store{:owners [#:store.owner{:user [:user/online?]}]} #:stream{:_store [:stream/state]} #:store{:locality [:sulo-locality/path]} #:store{:shipping [#:shipping{:rules [#:shipping.rule{:destinations [:country/code]}]}]} #:store{:status [:status/type]} :store/username]}]} #:query{:auth [:db/id :user/email #:user{:profile [:user.profile/name]}]} :query/current-route], :login [#:proxy{:login [:query/current-route :query/messages]} :query/auth0-info], :sell [#:query{:auth [:user/email :user/can-open-store? #:store.owner{:_user [#:store{:_owners [:db/id #:store{:profile [:store.profile/name]}]}]}]} #:query{:owned-store [:db/id]} #:query{:sulo-localities [:sulo-locality/title :db/id]} :query/messages], :product [#:query{:item [:db/id :store.item/name :store.item/price :store.item/index :store.item/not-found? #:store.item{:photos [#:store.item.photo{:photo [:photo/path :photo/id]} :store.item.photo/index]} #:store.item{:skus [:db/id :store.item.sku/variation :store.item.sku/inventory]} :store.item/description :store.item/section #:store.item{:category [:category/label :category/path :category/name]} #:store{:_items [#:store{:profile [#:store.profile{:photo [:photo/path :photo/id]} :store.profile/name :store.profile/tagline]} #:store{:owners [#:store.owner{:user [:user/online?]}]} #:stream{:_store [:stream/state]} #:store{:locality [:sulo-locality/path]} #:store{:shipping [#:shipping{:rules [#:shipping.rule{:destinations [:country/code]}]}]} #:store{:status [:status/type]} :store/username]}]}], :landing-page [:query/current-route #:query{:auth [:db/id]} #:query{:sulo-localities [:sulo-locality/title :sulo-locality/path #:sulo-locality{:photo [:photo/id]}]} #:query{:top-streams [:db/id :stream/title :stream/state #:stream{:store [:db/id #:store{:locality [:sulo-locality/path]} #:store{:profile [:store.profile/name #:store.profile{:photo [:photo/path :photo/id]}]} #:store{:status [:status/type]} :store/visitor-count :store/username]}]} :query/messages], :not-found [#:query{:featured-items [:db/id :store.item/name :store.item/price :store.item/index :store.item/not-found? #:store.item{:photos [#:store.item.photo{:photo [:photo/path :photo/id]} :store.item.photo/index]} #:store.item{:skus [:db/id :store.item.sku/variation :store.item.sku/inventory]} :store.item/description :store.item/section #:store.item{:category [:category/label :category/path :category/name]} #:store{:_items [#:store{:profile [#:store.profile{:photo [:photo/path :photo/id]} :store.profile/name :store.profile/tagline]} #:store{:owners [#:store.owner{:user [:user/online?]}]} #:stream{:_store [:stream/state]} #:store{:locality [:sulo-locality/path]} #:store{:shipping [#:shipping{:rules [#:shipping.rule{:destinations [:country/code]}]}]} #:store{:status [:status/type]} :store/username]}]} :query/current-route], :stores [#:query{:stores [:db/id #:store{:profile [:store.profile/name :store.profile/tagline #:store.profile{:photo [:photo/path :photo/id]}]} #:store{:locality [:sulo-locality/path]} #:store{:status [:status/type]} #:stream{:_store [:stream/state]} :store/visitor-count :store/username :store/created-at :store/featured #:store{:owners [#:store.owner{:user [:user/online?]}]}]}], :live [#:query{:streams [:db/id :stream/title :stream/state #:stream{:store [:db/id #:store{:locality [:sulo-locality/path]} #:store{:profile [:store.profile/name #:store.profile{:photo [:photo/path :photo/id]}]} #:store{:status [:status/type]} :store/visitor-count :store/username]}]} #:query{:stores [:db/id #:store{:profile [:store.profile/name :store.profile/tagline #:store.profile{:photo [:photo/path :photo/id]}]} #:store{:locality [:sulo-locality/path]} #:store{:status [:status/type]} #:stream{:_store [:stream/state]} :store/visitor-count :store/username :store/created-at :store/featured #:store{:owners [#:store.owner{:user [:user/online?]}]}]} #:query{:online-stores [:db/id #:store{:profile [:store.profile/name :store.profile/tagline #:store.profile{:photo [:photo/path :photo/id]}]} #:store{:locality [:sulo-locality/path]} #:store{:status [:status/type]} #:stream{:_store [:stream/state]} :store/visitor-count :store/username :store/created-at :store/featured #:store{:owners [#:store.owner{:user [:user/online?]}]}]}], :help [#:query{:auth [:user/email]} :query/current-route], :user [#:query{:auth [:db/id :user/email #:user{:profile [#:user.profile{:photo [:photo/path :photo/id]} :user.profile/name]}]} #:proxy{:order [:query/current-route #:query{:order [:db/id :order/uuid :order/status :order/amount #:order{:items [:order.item/type :order.item/amount :order.item/description :order.item/title #:order.item{:parent [#:store.item{:_skus [:store.item/name :store.item/price #:store.item{:photos [#:store.item.photo{:photo [:photo/id]} :store.item.photo/index]}]}]}]} #:order{:shipping [:shipping/name #:shipping{:address [:shipping.address/street :shipping.address/postal :shipping.address/locality :shipping.address/region #:shipping.address{:country [:country/name :country/code]}]}]} :order/created-at :order/user #:order{:charge [:db/id :charge/id]} #:order{:store [#:store{:profile [#:store.profile{:photo [:photo/id]} #:store.profile{:cover [:photo/id]} :store.profile/email :store.profile/tagline :store.profile/name]} #:store{:owners [#:store.owner{:user [:user/email]}]}]}]} #:query{:auth [:user/email]} #:query{:order-payment [:charge/id :charge/source :charge/created :charge/amount :charge/amount-refunded]}]} #:proxy{:order-list [:query/current-route #:query{:orders [:db/id :order/uuid :order/status :order/amount #:order{:items [:order.item/type :order.item/amount :order.item/description :order.item/title #:order.item{:parent [:store.item.sku/variation #:store.item{:_skus [:store.item/name :store.item/price #:store.item{:photos [#:store.item.photo{:photo [:photo/id]} :store.item.photo/index]}]}]}]} #:order{:shipping [:shipping/name #:shipping{:address [:shipping.address/street :shipping.address/postal :shipping.address/locality :shipping.address/region :shipping.address/country]}]} :order/created-at :order/user #:order{:store [#:store{:profile [#:store.profile{:photo [:photo/id]} #:store.profile{:cover [:photo/id]} :store.profile/name]}]}]} #:query{:featured-items [:db/id :store.item/name :store.item/price :store.item/index :store.item/not-found? #:store.item{:photos [#:store.item.photo{:photo [:photo/path :photo/id]} :store.item.photo/index]} #:store.item{:skus [:db/id :store.item.sku/variation :store.item.sku/inventory]} :store.item/description :store.item/section #:store.item{:category [:category/label :category/path :category/name]} #:store{:_items [#:store{:profile [#:store.profile{:photo [:photo/path :photo/id]} :store.profile/name :store.profile/tagline]} #:store{:owners [#:store.owner{:user [:user/online?]}]} #:stream{:_store [:stream/state]} #:store{:locality [:sulo-locality/path]} #:store{:shipping [#:shipping{:rules [#:shipping.rule{:destinations [:country/code]}]}]} #:store{:status [:status/type]} :store/username]}]}]} :query/current-route], :store-dashboard [#:query{:store [:db/id :store/uuid #:store{:profile [:store.profile/description :store.profile/name :store.profile/tagline :store.profile/return-policy #:store.profile{:cover [:photo/id]} #:store.profile{:photo [:photo/path :photo/id]}]} #:store{:owners [#:store.owner{:user [:user/email]}]} :store/stripe #:store{:sections [:db/id :store.section/label]} #:order{:_store [:order/items]} #:store{:items [:store.item/name :store.item/description :store.item/price :store.item/index #:store.item{:section [:db/id :store.section/label]} #:store.item{:photos [#:store.item.photo{:photo [:photo/path :photo/id]} :store.item.photo/index]} #:store.item{:skus [:db/id :store.item.sku/inventory :store.item.sku/variation]}]} #:stream{:_store [:stream/state]} #:store{:shipping [:shipping/policy]}]} :query/current-route :query/messages #:routing{:store-dashboard {:store-dashboard/order [:query/messages #:query{:order [:db/id #:order{:items [:order.item/type :order.item/amount :order.item/description :order.item/title #:order.item{:photo [:photo/id]} #:order.item{:parent [:store.item.sku/variation #:store.item{:_skus [:db/id :store.item/name :store.item/price]}]}]} :order/amount :order/status :order/created-at #:order{:charge [:charge/id :charge/amount]} #:order{:user [:user/email]} #:order{:shipping [:shipping/name #:shipping{:address [:shipping.address/country :shipping.address/street :shipping.address/street2 :shipping.address/postal :shipping.address/locality :shipping.address/region]}]} #:order{:store [#:store{:profile [:store.profile/name #:store.profile{:photo [:photo/path]}]}]}]} :query/current-route], :store-dashboard/create-product [:query/current-route :query/messages #:query{:navigation [:category/name :category/label :category/path :category/route-map]} #:query{:categories [:db/id :category/name :category/label :category/_children #:category{:children [:db/id :category/name :category/label :category/_children #:category{:children [:db/id :category/name :category/label :category/_children]}]}]} #:query{:item [:store.item/name :store.item/description :store.item/price #:store.item{:section [:db/id :store.section/label]} #:store.item{:skus [:db/id :store.item.sku/inventory :store.item.sku/variation]} #:store.item{:photos [#:store.item.photo{:photo [:photo/id]} :store.item.photo/index]} #:store.item{:category [:category/label :category/path :db/id #:category{:_children [:db/id :category/label :category/path #:category{:_children [:db/id :category/label :category/path]}]}]}]}], :store-dashboard/create-order [:query/messages #:query{:order [:db/id #:order{:items [:order.item/type :order.item/amount :order.item/description :order.item/title #:order.item{:photo [:photo/id]} #:order.item{:parent [:store.item.sku/variation #:store.item{:_skus [:db/id :store.item/name :store.item/price]}]}]} :order/amount :order/status :order/created-at #:order{:charge [:charge/id :charge/amount]} #:order{:user [:user/email]} #:order{:shipping [:shipping/name #:shipping{:address [:shipping.address/country :shipping.address/street :shipping.address/street2 :shipping.address/postal :shipping.address/locality :shipping.address/region]}]} #:order{:store [#:store{:profile [:store.profile/name #:store.profile{:photo [:photo/path]}]}]}]} :query/current-route], :store-dashboard/order-list [#:query{:orders [:order/store :order/uuid :order/status #:order{:items [:order.item/amount :order.item/description :order.item/type]} #:order{:shipping [:shipping/name]} :order/amount :order/created-at]} :query/current-route], :store-dashboard/profile [:query/current-route :query/messages #:proxy{:status [#:query{:store [#:store{:status [:status/type]} #:store{:profile [:store.profile/photo :store.profile/cover :store.profile/email]} #:store{:shipping [:shipping/rules]} :store/username :store/items #:store{:stripe [#:stripe{:status [:status/type]}]}]} #:query{:stripe-account [:stripe/details-submitted? :stripe/charges-enabled? :stripe/payouts-enabled? :stripe/verification]} :query/current-route :query/messages]}], :store-dashboard/product-list [#:query{:inventory [:store.item/name :store.item/description :store.item/index :store.item/price #:store.item{:section [:db/id]} #:store.item{:photos [#:store.item.photo{:photo [:photo/id :photo/path]} :store.item.photo/index]}]} #:query{:store [#:store{:sections [:db/id :store.section/label]}]} #:query{:ui-state [:ui.singleton.state/product-view]} :query/messages :query/current-route], :store-dashboard/finances [#:proxy{:settings [:query/messages #:query{:stripe-country-spec [:supported-bank-account-currencies]} #:query{:stripe-account [:stripe/id :stripe/external-accounts :stripe/default-currency :stripe/payout-schedule]} #:query{:store [:db/id]}]} #:proxy{:taxes [:query/current-route #:query{:store [:db/id #:store{:tax [:tax/automatic? #:tax{:rules [:tax.rule/rate :tax.rule/include-shipping?]}]}]} :query/messages]} #:query{:stripe-balance [:stripe/balance]} :query/current-route], :store-dashboard/business [#:query{:stripe-account [:stripe/id :stripe/country :stripe/verification :stripe/legal-entity :stripe/external-accounts :stripe/default-currency :stripe/payout-schedule :stripe/details-submitted?]} #:proxy{:verify [:query/stripe-country-spec :query/messages]} #:proxy{:finances [#:proxy{:settings [:query/messages #:query{:stripe-country-spec [:supported-bank-account-currencies]} #:query{:stripe-account [:stripe/id :stripe/external-accounts :stripe/default-currency :stripe/payout-schedule]} #:query{:store [:db/id]}]} #:proxy{:taxes [:query/current-route #:query{:store [:db/id #:store{:tax [:tax/automatic? #:tax{:rules [:tax.rule/rate :tax.rule/include-shipping?]}]}]} :query/messages]} #:query{:stripe-balance [:stripe/balance]} :query/current-route]} #:query{:auth [:user/email]} #:query{:store [:db/id #:store{:profile [:store.profile/email :store.profile/name]}]} :query/current-route :query/messages], :store-dashboard/shipping [:query/current-route :query/messages #:query{:countries [:country/code :country/name #:country{:continent [:continent/code :continent/name]}]} #:query{:store [#:store{:shipping [#:shipping{:rules [:db/id :shipping.rule/title #:shipping.rule{:rates [:db/id :shipping.rate/title :shipping.rate/info :shipping.rate/first :shipping.rate/additional :shipping.rate/free-above]} #:shipping.rule{:destinations [:country/code :country/name]}]} #:shipping{:address [:shipping.address/street :shipping.address/street2 :shipping.address/postal :shipping.address/locality :shipping.address/region #:shipping.address{:country [:country/code]}]}]}]}], :store-dashboard/stream [:query/messages #:query{:stream [:stream/state :stream/store :stream/token]} #:proxy{:chat [#:query{:chat [:chat/store :chat/modes #:chat{:messages [:chat.message/client-side-message? :chat.message/id #:chat.message{:user [:user/email :db/id #:user{:profile [#:user.profile{:photo [:photo/id]} :user.profile/name]}]} :chat.message/text :chat.message/created-at]}]}]} #:query{:auth [:db/id :user/email]} :query/current-route], :store-dashboard/product [:query/current-route :query/messages #:query{:navigation [:category/name :category/label :category/path :category/route-map]} #:query{:categories [:db/id :category/name :category/label :category/_children #:category{:children [:db/id :category/name :category/label :category/_children #:category{:children [:db/id :category/name :category/label :category/_children]}]}]} #:query{:item [:store.item/name :store.item/description :store.item/price #:store.item{:section [:db/id :store.section/label]} #:store.item{:skus [:db/id :store.item.sku/inventory :store.item.sku/variation]} #:store.item{:photos [#:store.item.photo{:photo [:photo/id]} :store.item.photo/index]} #:store.item{:category [:category/label :category/path :db/id #:category{:_children [:db/id :category/label :category/path #:category{:_children [:db/id :category/label :category/path]}]}]}]}], :store-dashboard/order-list-new [#:query{:orders [:order/store :order/uuid :order/status #:order{:items [:order.item/amount :order.item/description :order.item/type]} #:order{:shipping [:shipping/name]} :order/amount :order/created-at]} :query/current-route], :store-dashboard/order-list-fulfilled [#:query{:orders [:order/store :order/uuid :order/status #:order{:items [:order.item/amount :order.item/description :order.item/type]} #:order{:shipping [:shipping/name]} :order/amount :order/created-at]} :query/current-route], :store-dashboard [#:query{:store [:db/id :store/uuid :store/username #:store{:profile [:store.profile/name :store.profile/description #:store.profile{:photo [:photo/path :photo/id]}]} #:store{:owners [#:store.owner{:user [:user/email]}]} #:store{:stripe [#:stripe{:status [:status/type]}]} #:store{:status [:status/type]} #:store{:shipping [:shipping/rules]} #:order{:_store [:order/items]} #:stream{:_store [:stream/state]}]} :query/store-item-count #:query{:stripe-account [:stripe/details-submitted?]} #:query{:store-has-streamed [:ui.singleton.state/store-has-streamed?]} :query/current-route], :store-dashboard/policies [:query/current-route :query/messages #:proxy{:status [#:query{:store [#:store{:status [:status/type]} #:store{:profile [:store.profile/photo :store.profile/cover :store.profile/email]} #:store{:shipping [:shipping/rules]} :store/username :store/items #:store{:stripe [#:stripe{:status [:status/type]}]}]} #:query{:stripe-account [:stripe/details-submitted? :stripe/charges-enabled? :stripe/payouts-enabled? :stripe/verification]} :query/current-route :query/messages]}]}} #:query{:stripe-account [:stripe/legal-entity :stripe/verification :stripe/details-submitted? :stripe/charges-enabled? :stripe/payouts-enabled?]}], :about [#:query{:auth [:db/id]}]}}])

  (:stores (:routing/app-root (nth bad-query 4)))

  (def routing-app-root (nth bad-query 4))
  (def p (parser {:union-namespace "routing"
                       :join-namespace  "proxy"
                       :union-selector  (constantly :stores)}))
  (dedupe-query (p) {} bad-query)
  (p {}  )
  )