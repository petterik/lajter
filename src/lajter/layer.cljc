(ns lajter.layer
  (:require
    [lajter.protocols :as p]
    [lajter.logger :refer [log]]
    [lajt.parser :as parser]
    [clojure.spec.alpha :as s]
    [clojure.set :as set]

    [clojure.data :as data]))

(s/def ::remote-target keyword?)
(s/def ::reads-only-query ::parser/read-exprs)
(s/def ::mutations-only-query
  (s/coll-of ::parser/mutation-expr :kind vector? :gen-max 3))

(s/def :layer.remote/reads (s/map-of ::remote-target ::reads-only-query))
(s/def :layer.remote/mutates (s/map-of ::remote-target ::mutations-only-query))
(s/def :layer.local/mutates ::mutations-only-query)
(s/def :layer.merge/value any?)
(s/def :layer/id uuid?)
(s/def :layer.snapshot/db any?)
;; Convenience
(s/def :layer.remote/keys (s/coll-of (s/or :mutate symbol? :read keyword?) :kind set?))
(s/def :layer.remote/targets (s/coll-of ::remote-target :kind set?))

(s/def ::layer-map (s/keys :req [(or :layer.remote/mutates
                                     :layer.local/mutates
                                     :layer.merge/value
                                     :layer.remote/reads)]
                           :opt [:layer/id
                                 :layer.snapshot/db
                                 :layer.remote/keys
                                 :layer.remote/targets]))

(defn ->merge-layer [layer value]
  (merge {:layer.merge/value value}
         (dissoc layer
                 :layer.remote/targets
                 :layer.remote/mutates
                 :layer.remote/keys
                 :layer.remote/reads)))

(defn ->snapshot-layer [db]
  {:layer.snapshot/db db})

(defn- mutation-key? [k]
  (s/valid? ::parser/mutation-id k))

(defn- read-key? [k]
  (s/valid? ::parser/read-id k))

(defn- filter-query [pred query]
  (lajt.parser/update-query (filter pred) query))

(defn- split-query-by [pred coll]
  [(filter-query pred coll)
   (filter-query (complement pred) coll)])

(defn reducible-concat [& colls]
  (eduction cat colls))

(defn query-keys [query]
  (lajt.parser/query-into
    #{}
    (map ::parser/key)
    query))

(defn ->remote-layer [parser remotes env query]
  (let [[mutations reads]
        (->> query
             (split-query-by (comp mutation-key? ::parser/key))
             (map (fn [query]
                    (into {}
                          (comp
                            (map (juxt identity (partial parser env query)))
                            (remove (comp empty? second)))
                          remotes))))]
    {:layer.remote/reads   reads
     :layer.remote/mutates mutations
     :layer.remote/targets (set (reducible-concat (keys reads)
                                                  (keys mutations)))
     :layer.remote/keys    (into #{}
                                 (mapcat query-keys)
                                 (reducible-concat (vals reads)
                                                   (vals mutations)))}))

(defn ->local-layer [remote-layer query]
  (let [remote-mutations (:layer.remote/keys remote-layer)]
    {:layer.local/mutates
     (lajt.parser/update-query
       (filter (fn [{::parser/keys [key]}]
                 (and (mutation-key? key)
                      (not (contains? remote-mutations key)))))
       query)}))

(defn transaction-layer
  ([reconciler tx]
   (transaction-layer (:config reconciler) (p/to-env reconciler) tx))
  ([config env query]
   (let [{:keys [parser remotes]} config
         remote-layer (->remote-layer parser remotes env query)
         local-layer (->local-layer remote-layer query)]
     (-> (merge remote-layer local-layer)
         (assoc :layer/query query
                :layer/query-params
                (second (lajt.parser/separate-query-from-params query))
                :layer/mutates (lajt.parser/update-query
                                 (filter (comp mutation-key? ::parser/key))
                                 query))))))

(defn with-id [layer tx-id]
  (assoc layer :layer/id tx-id))

(defn with-query-params [layer query-params]
  (update layer :layer/query-params #(merge-with merge % query-params)))

(defn to-remote-query [layer target]
  (cond-> (into (get-in layer [:layer.remote/mutates target] [])
                (get-in layer [:layer.remote/reads target]))
          (seq (:layer/query-params layer))
          (list (:layer/query-params layer))))

(defn db-with-layers [db reconciler layers]
  (let [{:keys [parser] :as env} (p/to-env reconciler)
        mutate-db (fn [db query]
                    (if (empty? query)
                      db
                      (let [state (atom db)]
                        (parser (assoc env :state state :db db) query)
                        @state)))
        merge-db (fn [db to-merge]
                   ((:merge-fn (:config reconciler)) reconciler db to-merge nil))
        apply-layer (fn [db layer]
                      ;; If the layer contains something to merge.
                      ;; Apply the local mutations then call merge.
                      (if-let [to-merge (not-empty (:layer.merge/value layer))]
                        (-> db
                            (mutate-db (:layer.local/mutates layer))
                            (merge-db to-merge))
                        ;; Otherwise, just call all the mutates.
                        (mutate-db db (:layer/mutates layer))))]
    (reduce apply-layer db layers)))

;; Layer collection manipulations

(defn- update-layer [layers id f]
  (into []
        (map #(if (= id (:layer/id %)) (f %) %))
        layers))

(defn replace-layer [layers layer-id with-layer]
  (update-layer layers layer-id (constantly with-layer)))

(defn init-layers []
  [])

(defn add-layer [layers layer]
  (conj (or layers []) layer))

(def remote-layer? (comp seq :layer.remote/targets))

(defn take-until [pred coll]
  (transduce (halt-when pred (fn [r h] (conj r h)))
             conj
             []
             coll))


(defn top-layers
  "Returns layers on top of the latest snapshot"
  [layers]
  (let [ls (take-until (comp some? :layer.snapshot/db) (rseq layers))
        snapshot (peek ls)
        layers (rseq (pop ls))]
    {:layers   layers
     :snapshot snapshot}))

(defn remove-snapshots-after [layers layer-id]
  (let [[before [found & after]]
        (split-with (complement (comp #{layer-id} :layer/id)) layers)]
    (-> (into [] before)
        (cond-> (some? found) (conj found))
        (into (remove #(contains? % :layer.snapshot/db)) after))))

(comment
  (require '[clojure.spec.gen.alpha :as gen])
  (def layers (into [] (gen/sample (s/gen ::layer-map))))

  (count layers)
  (some #(when (:layer.snapshot/db %) %) layers)



  )

(defn first-remote-unsent [layers]
  (first
    (eduction
      (filter remote-layer?)
      (remove ::sent)
      (take 1)
      layers)))

(defn mark-sent-layer [layers layer]
  (update-layer layers (:layer/id layer) #(assoc % ::sent true)))