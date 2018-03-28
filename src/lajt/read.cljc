(ns lajt.read
  (:require
    [lajt.read.ops :as ops]
    #?(:clj
    [clojure.data])))

(def ^:dynamic *debug*)

(def default-ops
  {:pre     [:depends-on :params :before]
   :actions [:query :lookup-ref]
   :post    [:sort ::ops/pull]})

(defn- call-op [env k v]
  (let [ret (ops/call-op env k v)]
    #?(:clj
       (when *debug*
         (let [[before after] (clojure.data/diff env ret)]
           (prn {:op     k
                 :before before
                 :after  after}))))
    ret))

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

(defn- perform-ops [env ops]
  (reduce (fn [{:keys [read-map] :as env} read-op]
            (cond-> env
                    (contains? read-map read-op)
                    (call-op read-op
                             (get read-map read-op))))
          env
          ops))

(defn perform-read [{:keys [reads read-key read-ops] :as env}]
  (validate-read! env)
  (let [
        ;; Adds read-map with an additional ::ops/pull action
        ;; whenever there's a query.
        env (cond-> (assoc env :read-map (reads read-key))
                    (not-empty (:query env))
                    (assoc-in [:read-map ::ops/pull] (:query env)))
        ;; Performs the :pre ops
        env (perform-ops env (:pre read-ops))
        ;; Call an :action
        action (get-action env)
        env (call-op env action (get-in env [:read-map action]))
        ;; Call :post ops
        env (perform-ops env (:post read-ops))]
    env))

(defn om-next-value-wrapper [read]
  (fn [env k p]
    {(or (:target env) :value) (read env k p)}))

(defn ->read-fn [lajt-reads db-fns]
  (fn [env k p]
    (binding [*debug* (:debug env false)]
      (let [env (assoc env :params p
                           :read-key k
                           :reads lajt-reads
                           :db-fns db-fns
                           :read-ops (:read-ops env default-ops))]
        (if-let [remote (:target env)]
          (get (lajt-reads k) remote)
          (ops/get-result (perform-read env)))))))


