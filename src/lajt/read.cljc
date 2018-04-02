(ns lajt.read
  (:require
    [lajt.read.ops :as ops]))

(def default-ops
  {:pre     [:case :depends-on :params :before]
   :actions [:query :lookup-ref :no-op]
   :post    [:sort ::ops/pull]})

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
  (let [after (cond-> env
                      (contains? read-map read-op)
                      (ops/call read-op (get read-map read-op)))]
    (if (= (:read-map env)
           (:read-map after))
      after
      (reduced {::new-map after}))))

(defn- perform-ops [env ops]
  (reduce perform-op env ops))

(defn- perform-read* [{:keys [read-ops] :as env}]
  (let [
        ;; Performs the :pre ops
        env (perform-ops env (:pre read-ops))]
    ;; Check if :pre-ops changed the read-map
    (if-some [nm (::new-map env)]
      (recur nm)
      ;; Perform the action and the :post ops.
      (let [action (get-action env)
            ;; Adds read-map with an additional ::ops/pull action
            ;; whenever there's a query.
            env (cond-> env
                        (not-empty (:query env))
                        (assoc-in [:read-map ::ops/pull] (:query env)))
            env (ops/call env action (get-in env [:read-map action]))]
        (perform-ops env (:post read-ops))))))

(defn perform-read [{:keys [reads read-key] :as env}]
  (validate-read! env)
  (perform-read* (assoc env :read-map (reads read-key))))

(defn om-next-value-wrapper [read]
  (fn [env k p]
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


