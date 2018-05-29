(ns lajter.layer
  (:require
    [lajter.protocols :as p]
    [lajt.parser :as parser]
    [clojure.spec.alpha :as s]))

(s/def ::remote-target keyword?)
(s/def ::reads-only-query ::parser/read-exprs)
(s/def ::mutations-only-query
  (s/coll-of ::parser/mutation-expr :kind vector? :gen-max 3))

(s/def :layer.remote/reads (s/map-of ::remote-target ::reads-only-query))
(s/def :layer.remote/mutates (s/map-of ::remote-target ::mutations-only-query))
(s/def :layer.local/mutates ::mutations-only-query)
(s/def :layer.merge/value any?)
(s/def :layer/id uuid?)
;; Convenience
(s/def :layer.remote/keys (s/coll-of (s/or :mutate symbol? :read keyword?) :kind set?))
(s/def :layer.remote/targets (s/coll-of ::remote-target :kind set?))

(s/def ::layer-map (s/keys :req [(or :layer.remote/mutates
                                     :layer.local/mutates
                                     :layer.merge/value
                                     :layer.remote/reads)]
                           :opt [:layer/id
                                 :layer.remote/keys
                                 :layer.remote/targets]))

(defn ->merge-layer [layer value]
  (merge {:layer.merge/value value}
         (dissoc layer
                 :layer.remote/targets
                 :layer.remote/mutates
                 :layer.remote/keys
                 :layer.remote/reads)))

(defn- mutation-key? [k]
  (s/valid? ::parser/mutation-id k))

(defn- read-key? [k]
  (s/valid? ::parser/read-id k))

(defn- split-by [pred coll]
  [(filter pred coll)
   (remove pred coll)])

(defn ->remote-layer [parser remotes env parsed-query]
  (let [[mutations reads]
        (->> parsed-query
             (split-by (comp mutation-key? ::parser/key))
             (map lajt.parser/parsed-query->query)
             (map (fn [query]
                    (into {}
                          (comp
                            (map (juxt identity (partial parser env query)))
                            (remove (comp empty? second)))
                          remotes))))]
    {:layer.remote/reads   reads
     :layer.remote/mutates mutations
     :layer.remote/targets (set (concat (keys reads) (keys mutations)))
     :layer.remote/keys    (into #{}
                                 (comp (mapcat parser/query->parsed-query)
                                       (map ::parser/key))
                                 (concat (vals reads) (vals mutations)))}))

(defn ->local-layer [remote-layer parsed-query]
  (let [remote-mutations (:layer.remote/keys remote-layer)]
    {:layer.local/mutates
     (into []
           (comp (filter (fn [{::parser/keys [key]}]
                           (and (mutation-key? key)
                                (not (contains? remote-mutations key)))))
                 (lajt.parser/parsed-query->query))
           parsed-query)}))

(defn transaction-layer
  ([reconciler tx]
   (transaction-layer (:config reconciler) (p/to-env reconciler) tx))
  ([config env tx]
   (let [{:keys [parser remotes]} config
         parsed-query (lajt.parser/query->parsed-query tx)
         remote-layer (->remote-layer parser remotes env parsed-query)
         local-layer (->local-layer remote-layer parsed-query)]
     (-> (merge remote-layer local-layer)
         (assoc :layer/query tx
                :layer/mutates (into []
                                     (comp (filter (comp mutation-key? ::parser/key))
                                           (lajt.parser/parsed-query->query))
                                     parsed-query))))))

(defn with-id [layer tx-id]
  (assoc layer :layer/id tx-id))
