(ns lajt.read
  (:require
    [lajt.read.ops :as ops]))

(def default-ops
  {:pre     [:case :depends-on :params :before]
   :actions [:query :lookup-ref :custom :no-op]
   :post    [:sort ::ops/pull :after]})

(defn- validate-read! [{:keys [reads read-key] :as env}]
  (let [read-map (reads read-key)]
    (when (nil? read-map)
      (throw (ex-info (str "No such read: " read-key)
                      {:read-key read-key
                       :query    (:query env)})))))

(defn- get-action [{:keys [read-map read-key read-ops] :as env}]
  (let [actions (filterv (set (:actions read-ops)) (keys read-map))]
    (if (== 1 (count actions))
      (first actions)
      (throw
        (ex-info
          (if (zero? (count actions))
            (str "read-map did not contain any of the actions."
                 " It must contain one of: "
                 (:actions read-ops))
            (str "Cannot have multiple actions"
                 " in the read's map. Had actions: "
                 actions))
          {:actions  (:actions read-ops)
           :read-key read-key
           :read-map read-map
           :query    (:query env)})))))

(defn- perform-op [{:keys [read-map] :as env} read-op]
  (cond-> env
          (contains? read-map read-op)
          (ops/call read-op (get read-map read-op))))

(defn- perform-ops [env ops post-call-fn]
  (reduce (fn [env op]
            (post-call-fn env (perform-op env op)))
          env
          ops))

(defn- perform-read* [{:keys [read-ops] :as env}]
  (let [
        ;; Performs the :pre ops
        env (perform-ops env
                         (:pre read-ops)
                         (fn [env after]
                           (if (= (:read-map env) (:read-map after))
                             after
                             (reduced {::new-map after}))))]
    ;; Check if :pre-ops changed the read-map
    (if-some [nm (::new-map env)]
      (recur nm)
      (let [
            ;; Adds read-map with an additional ::ops/pull action
            ;; whenever there's a query.
            env (cond-> env
                        (not-empty (:query env))
                        (assoc-in [:read-map ::ops/pull] (:query env)))
            ;; Perform the action
            env (perform-op env (get-action env))
            assoc-result #(assoc % :result (ops/get-result %))]

        (perform-ops (assoc-result env)
                     (:post read-ops)
                     (fn [_ after]
                       (assoc-result after)))))))

(defn perform-read [{:keys [reads read-key] :as env}]
  (validate-read! env)
  (perform-read* (assoc env :read-map (reads read-key))))

(defn- wrap-query-in-join-ast [env remote-ret]
  (if-some [->ast (:om.next.parser.impl/expr->ast env)]
    (let [{:keys [join-namespace]} ((:parser env))
          join-key (keyword (name join-namespace) (name (gensym)))]
      (->ast {join-key remote-ret}))
    (throw (ex-info (str "Must pass :om.next/expr->ast to env when "
                         "using lajt.read/om-next-value-wrapper.")
                    {}))))

(defn om-next-value-wrapper [read]
  (fn [env k p]
    (let [ret (read env k p)]
      (if-some [t (:target env)]
        (cond
          (true? ret)
          {t ret}
          (sequential? ret)
          {t (wrap-query-in-join-ast env ret)}
          (fn? ret)
          {t (wrap-query-in-join-ast env (ret env))}
          :else
          (throw (ex-info (str "Unknown return value for read key: " k
                               " when parser was called with target: " t)
                          {:read-key k
                           :target   t
                           :query    (:query env)})))))
    {(or (:target env) :value) (read env k p)}))

(defn ->read-fn [lajt-reads db-fns]
  (fn [env k p]
    (binding [ops/*debug* (:debug env false)]
      (let [env (assoc env :params p
                           :read-key k
                           :reads lajt-reads
                           :db-fns db-fns
                           :read-ops (:read-ops env default-ops))]
        (if-let [remote (:target env)]
          (get (lajt-reads k) remote)
          (ops/get-result (perform-read env)))))))
