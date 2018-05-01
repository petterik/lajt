(ns lajt.read.ops
  (:require
    #?(:clj
        [clojure.data])
    [medley.core :as m]
    [clojure.spec.alpha :as s]
    [clojure.string :as string]
    [clojure.spec.gen.alpha :as gen]))

(def ^:dynamic *debug*)

(def ops (atom {}))

(s/def :op/type #{:op.type/setup
                  :op.type/action
                  :op.type/transform
                  ;; Special type can depend on the op.types them selves.
                  :op.type/special
                  ;; Remote type for returning remote data.
                  :op.type/remote
                  })
(s/def :op/key keyword?)
(s/def :op/dependents (s/+ keyword?))
(s/def :op/depends-on (s/+ keyword?))
;; Spec ::env somewhere such that we can have rules on what the different
;; :op/types are allowed/not allowed to do?
;; ::env is spec'ed in lajt.parser, but we should probably put it some where
;; better.
;; I feel I'm not getting a lot of value from clojure.spec right now.
;; Should probably be using instrument somewhere.
(s/def :op/fn (s/fspec :args (s/cat :env map? :value any?) :ret map?))
(s/def ::op (s/keys :req [:op/key :op/type :op/fn]
                    :opt [:op/dependents
                          :op/depends-on]))

(defn def-op! [m]
  (when-not (s/valid? ::op m)
    (throw (ex-info (s/explain-str ::op m)
                    (s/explain-data ::op m))))
  (if-let [k (:op/key m)]
    (swap! ops assoc k m)
    (prn "WARN: Op was not registered, as it missed :op/key. Was: " m)))

(defprotocol IOpStage
  (dependents [this])
  (depends-on [this])
  (stage-type [this])
  (call-stage [this env value]))

(extend-protocol IOpStage
  #?@(:clj
      [clojure.lang.AFn
       (dependents [this] [])
       (depends-on [this] [])
       (stage-type [this] (this))
       (call-stage [this env value]
         (this env value))
       clojure.lang.APersistentMap
       (dependents [this] (:op/dependents this))
       (depends-on [this] (:op/depends-on this))
       (stage-type [this] (:op/stage this))
       (call-stage [this env value]
         ((:op/fn this) env value))]
      :cljs
      [object
       (dependents [this] (if (map? this) (:op/dependents this) []))
       (depends-on [this] (if (map? this) (:op/depends-on this) []))
       (stage-type [this] (if (map? this) (:op/stage this) (this)))
       (call-stage [this env value]
                   (if (map? this)
                     ((:op/fn this) env value)
                     (this env value)))]))

(defn def-operation [op-key & args]
  ())

(def-operation :sort
  :op.stage/setup
  (fn [env value]
    )
  :op.stage/transform
  :op/dependents [:whatever]
  (fn [env value]
    ))

{:op/key    :sort
 :op/stages [{:op/stage      :op.stage/setup
              :op/dependents []
              :op/depends-on []
              :op/fn         (fn [env v])}
             {:op/stage      :op.stage/transform
              :op/dependents []
              :op/depends-on []
              :op/fn         (fn [env v])}]}
;; This is all we need for an op right now.
;; Need to know when it should be run (type).
;; We need to know if it should depend on other ops.
;; Need to know if it has other ops depending on it (dependents).
;; We need to be able to call the op.


(defn assoc-scoped [env k v]
  (assoc-in env [k (:read-key env)] v))

(defn update-scoped [env k & args]
  (apply update-in env [k (:read-key env)] args))

(defn get-scoped [env k]
  (get-in env [k (:read-key env)]))

(defn add-result
  "Adds a result to env. Convenicence function for actions."
  [env res]
  (assoc-scoped env ::results res))

(defn get-result
  [env]
  (get-scoped env ::results))

(defn remove-pull [env]
  (update env :read-map dissoc ::pull))

(def dispatch (fn [env k v] k))

;; ############
;; Remote data
;; TODO: We might want to move this out to its own namespace.

(defmulti remote-data dispatch :default ::default)

(defmethod remote-data ::default
  [env k v]
  (throw (ex-info (str "No read-op for key: " k)
                  {:key k :value v :query (:query env)})))

;; Hmm, maybe we want
{:pre (fn [])
 :remote (fn [])
 :post-action (fn [])
 :deps {:pre []
        :post-action []
        :remote []}}
{:action (fn [])}

;; ############
;; Ops
;; TODO: This def-op stuff is probably bad. Should probably remove it.
(defmulti def-op (fn [k] k) :default ::default)
(defmulti call-op dispatch :default ::default)

(defmethod def-op :depends-on [_] {:op-type :op-type/pre})
(defmethod call-op :depends-on
  [env _ v]
  (let [query (if (fn? v) (v env) v)
        res ((:parser env) env query (:target env))
        env (if (some? (:target env))
              (update env ::results (fnil into []) res)
              (update env ::results merge res))]
    ;; Assoc the :depends-on key with all results
    ;; such reads can access it easily.
    (assoc env :depends-on (::results env))))

(defn call-fns
  "Calls a single function or a vector of functions with the env."
  [x env]
  (when (some? x)
    (cond (fn? x) (x env)
          (vector? x) (reduce #(%2 %1) env x)
          :else
          (throw (ex-info (str "Unknown call chain: " x)
                          {:call-chain x
                           :env        env})))))

(defmethod def-op :before [_] {:op-type :op-type/pre})
(defmethod call-op :before
  [env _ v]
  (call-fns v env)
  env)

(defmethod def-op :params [_] {:op-type :op-type/pre})
(defmethod call-op :params
  [env _ params]
  (assoc env :params
             (when (some? params)
               (cond
                 (map? params)
                 (m/map-vals #(call-fns % env) params)

                 (fn? params)
                 (params env)
                 :else
                 (throw
                   (ex-info
                     ":params need to be a map or a function."
                     {:params params}))))))

(defmethod def-op :query [_] {:op-type :op-type/action})
(defmethod call-op :query
  [env k query]
  (let [q-params (:params env)
        values-set? (every? (comp some? val) q-params)]
    (if-not values-set?
      (do
        (when *debug*
          (locking *out*
            (prn "WARN: Not all params were set when executing query: "
                (:read-key env)
                " Returning nil.")))
        env)
      (let [query (update query :in #(into (vec (or % '[$]))
                                           (keys q-params)))
            res (apply (get-in env [:db-fns :q]) query (:db env)
                       (vals q-params))]

        #_(prn {:op     k
                :result res
                :query  query
                :params q-params})
        (add-result env res)))))

(defmethod def-op :lookup-ref [_] {:op-type :op-type/action})
(defmethod call-op :lookup-ref
  [env _ ref]
  (add-result env ((get-in env [:db-fns :entid]) (:db env) ref)))

(defmethod def-op :no-op [_] {:op-type :op-type/action})
(defmethod call-op :no-op
  [env _ _]
  (remove-pull env))

;; TODO: Make it expand from :sort -> :sort/pre :sort/post :sort/remote
;; are expansions different from pre?
;; why?
(defmethod def-op :sort [_] {:op-type :op-type/post})
(defmethod call-op :sort
  [env _ sort-map]
  (let [result (get-result env)
        {:keys [comparator key-fn order entities?]
         :or   {entities?  true
                comparator compare}} sort-map
        comparator (if (= :decending order)
                     (fn [a b]
                       (comparator b a))
                     comparator)
        !key-fn (if (and (some? key-fn) entities?)
                 (let [{:keys [entity]} (:db-fns env)
                       db (:db env)
                       entity* (memoize #(entity db %))]
                   (fn [a]
                     (key-fn (entity* a))))
                 key-fn)
        ret (if (or (map? result) (not (coll? result)))
              [result]
              (if (some? !key-fn)
                (sort-by !key-fn comparator result)
                (sort comparator result)))]
    (-> (add-result env ret)
        (cond-> (and entities? (keyword? key-fn))
                (assoc-scoped ::sort
                              {:remote-query
                               [{(:read-key env) [key-fn]}]})))))

;; ###############
;; Pull implementation

;; TODO: Move these specs somewhere?
(defn logic-var? [sym]
  (and (symbol? sym)
       (nil? (namespace sym))
       (string/starts-with? (name sym) "?")))

(s/def ::logic-var (s/with-gen logic-var?
                               #(gen/fmap (fn [sym] (symbol (str "?" (name sym))))
                                          (s/gen symbol?))))

(s/def ::aggregate (s/cat :fn symbol? :args (s/+ ::logic-var)))

(s/def ::find-element (s/or :logic-var ::logic-var
                            :aggregate ::aggregate))

(s/def ::find-pattern (s/or :relation (s/+ ::find-element)
                            :tuple (s/tuple (s/+ ::find-element))
                            :scalar (s/cat :find-element ::find-element
                                           :dot '#{.})
                            :collection (s/tuple (s/cat :find-element
                                                        ::find-element
                                                        :dots
                                                        '#{...}))))
(s/conform ::find-pattern '[(count ?e) .])

(defn- pull-fn-by-action-result [env]
  (let [res (get-result env)]
    (if (or (number? res)
            ;; Lookup ref results
            (and (sequential? res)
                 (== 2 (count res))
                 (keyword? (first res))
                 (some? (second res))))
      :pull
      (when (and (sequential? res)
                 (every? pos-int? res))
        :pull-many))))

(defn pull-type [env read-map]
  (cond
    ;; TODO: let actions define if they can pull or not?
    (some? (:lookup-ref read-map))
    (pull-fn-by-action-result env)

    (some? (:query read-map))
    (let [[type m] (s/conform ::find-pattern (get-in read-map [:query :find]))
          [find-element] (:find-element m)]
      ;; An aggregate can return any type of result.
      ;; Look for the find-pattern type in the result.
      (if (= :aggregate find-element)
        (pull-fn-by-action-result env)
        (condp = type
          :scalar :pull
          :collection :pull-many
          ;; TODO: Returning nil on :relation.
          ;; Is this what we want?
          nil)))

    (some? (:custom read-map))
    (pull-fn-by-action-result env)
    :else
    (throw (ex-info (str ":find-pattern-type not implemented for "
                         "read-map with keys: " (keys read-map))
                    ;; TODO: env->ex-data
                    {:read-map read-map}))))

(defmethod def-op ::pull [_] {:op-type :op-type/post})
(defmethod call-op ::pull
  [{:keys [read-map] :as env} _ query]
  (let [pull-type (pull-type env read-map)
        pull-fn (get-in env [:db-fns pull-type])
        result (get-result env)]
    (when (and (some? pull-type) (nil? pull-fn))
      (throw (ex-info (str "WARN: Cannot perform a pull on a query that"
                           " was not of :scalar or :collection find-pattern type.")
                      {:read-map  read-map
                       :query     query
                       :pull-type pull-type})))
    (add-result env (cond->> result
                             (and (some? result) (some? pull-fn))
                             (pull-fn (:db env) query)))))

;; base+case

(s/def ::fn-call (s/or :keyword keyword? :fn fn?))
(s/def ::fn-calls (s/coll-of ::fn-call :type vector? :gen-max 3))

(s/def ::case-pred (s/or :fn fn?
                         :case ::fn-calls))
(s/def ::case-preds (s/or
                      :one ::case-pred
                      :multiple (s/coll-of ::case-pred :type vector? :gen-max 3)))

;; TODO: The map here is really a query. Enter the query spec
;;       once we have one.
(s/def ::case-val map?)
(s/def ::case-map (s/map-of ::case-preds ::case-val
                            :conform-keys true
                            :count 1))
(s/def ::cases (s/+ ::case-map))

(comment

  (def -cases
    [{[[:route-params :petter] #(-> %)]
      {:query '{:where [[?e :person/first-name "Petter"]]}}}

     {[:route-params :diana]
      {:query '{:where [[?e :person/first-name "Diana"]]}}}

     {[[:route-params :diana]]
      {:query '{:where [[?e :person/first-name "Diana"]]}}
      }



     {#(-> % :foo) {}}])

  (s/conform ::cases -cases)
  (s/explain ::cases -cases)
  (s/conform ::case-map (first -cases))
  (s/conform ::case-pred (key (ffirst -cases)))
  (s/unform ::case-pred (s/conform ::case-pred (key (ffirst -cases))))
  )

(defn- find-case-match [env cases]
  (->> (s/conform ::cases cases)
       (some (fn [case-map]
               (let [[case-preds case-val] (first case-map)
                     preds (cond-> (second case-preds)
                                   (= :one (first case-preds))
                                   vector)
                     all? (->> preds
                               (map (fn [case-pred]
                                      (let [[type x] case-pred]
                                        (condp = type
                                          :fn (x env)
                                          :case (call-fns
                                                  (s/unform ::fn-calls x)
                                                  env)))))
                               (every? boolean))]

                 (when all?
                   (s/unform ::case-val case-val)))))))

(defmulti case-merge (fn [k o1 o2] k) :default ::default)
(defmethod case-merge ::default
  [_ o1 o2]
  (merge o1 o2))

(defn- merge-query [a b]
  ;; TODO: handle cases where a symbol is present in multiple queries.
  (-> (merge-with into
                  (dissoc a :find)
                  (dissoc b :find))
      (assoc :find (or (:find a) (:find b)))))

(defmethod case-merge :query
  [_ o1 o2]
  (merge-query o1 o2))

(defmethod call-op :case
  [{:keys [read-map target] :as env} _ cases]
  (let [base (:base read-map)
        match (find-case-match env cases)
        read-map (if (nil? match)
                   (cond-> {:no-op nil}
                           (contains? base target)
                           (assoc target (get base target)))
                   (reduce-kv (fn [m k right]
                                (if-some [[_ left] (find m k)]
                                  (assoc m k (case-merge k left right))
                                  (assoc m k right)))
                              base
                              match))]
    ;; Changes the read-map to be the new merged map.
    (assoc env :read-map read-map)))

(defmethod call-op :lastly
  [env _ calls]
  ;; :lastly sets the result to whatever the functions return.
  ;; Functions are passed the result as argument.
  (add-result env (call-fns calls env)))

(defmethod call-op :custom
  [env _ custom-fn]
  ;; Functions are passed the result as argument.
  (add-result env (custom-fn env)))

(defn call [env k v]
  #?(:cljs
     (call-op env k v)
     :clj
     (try
       (when *debug*
         (locking *out*
           (prn "Entering op: " k " for k: " (:read-key env))))
       (let [ret (call-op env k v)]
         (when *debug*
           (let [[before after] (clojure.data/diff env ret)]
             (locking *out*
               (prn {:op     k
                     :before before
                     :after  after}))))
         ret)
       (catch Throwable t
         (when *debug*
           (locking *out*
             (prn {:op     k
                   :before (select-keys env [:read-map :read-key :query])
                   :after  :exception
                   :ex     (Throwable->map t)})))
         (throw t)))))

(comment
  (let [ret nil]
    (when-not (map? ret)
      (throw (ex-info (str "Call to op: " k
                           " did not return a map."
                           " Must return a new environment")
                      {:op k :val v})))
    ret)


  {:sort
   {:pre (fn [])
    :post (fn [])
    :post-deps [:action]
    :remote (fn [])
    :remote-deps [:pre]}}

  {:deps {:pre nil
          :post [:target]
          :remote [:pre]
          :action [:pre]}}

  ;; Options:
  ;; 1. Define everything as multimethods
  ;; - these multimethods would have well defined semantics.
  ;; - pre-op are called first
  ;; - actions are called after all pre-ops.
  ;; - post-ops are called after actions or remote data
  ;; How there's also a dependency multimethod where one can specify ones dependencies?
  ;; so sort would be this mess:
  (defmethod pre-op :sort [_] :IMPLEMENTATION)
  (defmethod post-op :sort [_] :IMPLEMENTATION)
  (defmethod remote-op :sort [_] :IMPLEMENTATION)
  (defmethod dependencies :sort [_] :IMPLEMENTATION)



  (defprotocol IPreOp
    (pre [this env v]))
  (defprotocol IPostOp
    (pre [this env v]))
  (defprotocol IDependOnOps
    (pre [this env v]))
  (defprotocol IAction
    (pre [this env v]))
  (defprotocol IRemote
    ())

  ;; :sort
  (reify
    IPreOp
    (pre [this env v])
    IPostOp
    ()
    ;; got tired of typing...
    )

  ;; Or we could just write a macro around the multimethods
  (let [k->op {:pre pre-op
               :post post-op
               ;; ...
               }]
    (defmacro defop [op-id args & body]
     `(do
        ~@(map (fn [[k v]]
                 `(defmethod ~(get k->op k) ~op-id
                    ~args
                    ~v))
               (partition 2 body)))))

  ;; get it to work with multimethods.
  ;; write macro with specs for arguments.
  ;; done.

  ;; Or, we could just flatten everything out.
  ;; :sort is an expanding op that expands to
  ;; :sort/pre
  ;; :sort/post
  ;; :sort/remote

  ;; Everytime an expansion happens, the dependencies are re-calculated.
  ;; Should there be an expanding-op thing?
  ;; Or just ops as per ush

  ;; This should be a thing
  (defmulti op-dependency (fn [k] k) :default ::default)
  (defmethod op-dependency :sort
    []
    {:op-type :op-type/expansion
     ::depends-on [:case]})
  :op-types #{:op-type/expansion :op-type/pre :op-type/action
               :op-type/remote :op-type/post}

  )