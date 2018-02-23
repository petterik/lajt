(ns lime.parser
  (:require [clojure.spec.alpha :as s]))

;; TODO: Support idents?
(s/def ::id keyword?)
(s/def ::reads (s/coll-of ::read-expr :kind vector?))
(s/def ::join (s/map-of ::id ::reads :count 1))
(s/def ::union-map (s/map-of keyword? (s/coll-of ::read-expr :kind vector?)))
(s/def ::union (s/map-of keyword? ::union-map :count 1))

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

(defn- dispatch [env k]
  (let [call-fn (get env (::type env))]
    ;; returning a pair that can be conjed into a {}.
    [k
     (call-fn (dissoc env :read :mutate)
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
  [env [_ k]]
  (dispatch env k))

(defmethod parse :join
  [env [_ m]]
  (let [[k v] (first m)]
    (dispatch (assoc env :query (s/unform ::reads v))
              k)))

(defmethod parse :union
  [env [_ m]]
  (let [[k v] (first m)]
    (dispatch (assoc env :query (s/unform ::union-map v))
              k)))

(defmethod parse :mutate
  [env [_ {:keys [id params]}]]
  (dispatch (assoc env ::type :mutate :params params)
            id))

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
             (map (partial parse {:read prn :mutate prn})))))