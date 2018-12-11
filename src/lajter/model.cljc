(ns lajter.model
  (:require
    [clojure.spec.alpha :as s]
    [clojure.string :as string]
    [datascript.core :as d]
    [medley.core :as m]))

(s/def ::model
  (s/* (s/cat :sym symbol?
              :selectors (s/? (s/and coll? ::model)))))

(def primitive-types
  '#{String Boolean Integer Float ID})

(def primitive-type? #(contains? primitive-types %))
(def reference-type? (every-pred some?
                                 (complement primitive-type?)))

(def model-schema
  {:model.type/symbol {:db/unique :db.unique/identity}
   :model.node/symbol {:db/index true}
   :model.node/parent {:db/valueType :db.type/ref}
   :model.node/meta   {:db/valueType :db.type/ref}})

(defn init-meta-db
  ([] (init-meta-db nil))
  ([schema]
   (d/db (d/create-conn (merge-with merge model-schema schema)))))

(declare q)

(defn find-root [meta-db sym]
  (q '{:find  [?e .]
         :in    [$ ?sym]
         :where [(root-node ?e ?sym)]}
       meta-db
       sym))

(defn find-node [meta-db sym parent]
  (q '{:find  [?e .]
       :in    [$ ?sym ?parent]
       :where [[?e :model.node/parent ?parent]
               [?e :model.node/symbol ?sym]]}
     meta-db
     sym
     parent))

(defn find-meta [meta-db node-id]
  (-> (d/entity meta-db node-id)
      (:model.node/meta)
      (:db/id)))

(defn- temp-id [] (d/tempid :db.part/user))
(defn- temp-id? [id]
  (neg? id))

(defn comp-pipeline [& pipelines]
  (reduce (fn [p1 p2]
            (fn [opts]
              (into (p1 opts) (p2 opts))))
          pipelines))

(defn default-merge-meta-fn
  "Default implementation of handling merge conflicts when
  merging meta data for a node. Returning the new value if
  not nil."
  [env old-value new-value]
  (or new-value old-value))

(defn default-pipeline
  "Pipeline taking a flattened model, linked by db ids and
  returns nodes to transacted in to the model database.

  The idea is that we can use this pipeline to hook in to
  whatever we might need in the future as far as merging
  data goes."
  [{:keys [db merge-meta-fn]
    :or   {merge-meta-fn default-merge-meta-fn}
    :as   pipeline-opts}]
  [(map (fn [{:keys [id sym parent]}]
          (let [m (meta sym)]
            (cond-> {:db/id             id
                     :model.node/symbol sym}
                    (seq m)
                    (assoc :model.node/meta m)
                    (some? parent)
                    (assoc :model.node/parent parent)))))
   ;; needs is-capitalized attribute to figure out whether
   ;; the node is its own type definition or not.
   (let [capitalized?
         (fn [sym]
           (let [c (first (name sym))]
             #?(:clj  (Character/isUpperCase (char c))
                :cljs (= c (.toUpperCase c)))))]
     (map #(assoc % :model.node/capitalized?
                    (capitalized? (:model.node/symbol %)))))

   ;; Merge metadata.
   (map (fn [node]
          (let [node-meta (:model.node/meta node)
                meta-id (or (find-meta db (:db/id node))
                            (temp-id))
                old-meta (d/entity db meta-id)

                merge-fn
                (fn [[k v]]
                  (if-some [old-v (get old-meta k)]
                    (let [env (assoc pipeline-opts :k k :node node)]
                      (merge-meta-fn env old-v v))
                    v))

                new-meta
                (cond->> node-meta
                         (not (temp-id? meta-id))
                         (into {} (map (juxt key merge-fn))))]
            (cond-> node
                    (seq new-meta)
                    (assoc :model.node/meta
                           (assoc new-meta :db/id meta-id))))))])

(defn- root->txs [meta-db {:keys [sym selectors] :as m}]
  (let [add-node-id
        (fn self [parent {:keys [sym] :as m}]
          (let [id (or (find-node meta-db sym parent)
                       (temp-id))]
            (-> (assoc m :id id :parent parent)
                (update :selectors #(map (partial self id) %)))))
        id (or (find-root meta-db sym)
               (temp-id))]
    (cons (assoc m :id id)
          (map (partial add-node-id id) selectors))))

(defn index-model
  ([meta-db model]
   (index-model meta-db model {:pipeline default-pipeline}))
  ([meta-db model {:keys [pipeline pipeline-opts]}]
   (let [pipeline (comp-pipeline default-pipeline pipeline)]
     (reduce (fn [meta-db root]
               (let [xf (apply comp (pipeline (assoc pipeline-opts :db meta-db)))]
                 (->> (root->txs meta-db root)
                      (sequence xf)
                      (d/db-with meta-db))))
             meta-db
             (s/conform ::model model)))))

(defn find-index [pred coll]
  (->> coll
       (keep-indexed (fn [idx x] (when (pred x) idx)))
       (first)))

(defn- query-with-rules [query-map inputs rules]
  (let [inputs (vec inputs)
        rule-idx (find-index #{'%} inputs)
        inputs (if rule-idx
                 (update  rule-idx into rules)
                 (into inputs [rules]))
        in (cond-> (or (some-> (:in query-map) (not-empty) (vec))
                       '[$])
                   (not rule-idx)
                   (into '[%]))]
    [inputs (assoc query-map :in (vec in))]))

(def query-rules
  ;; Bind ?e to root node matching symbol.
  '[[(root-node ?e ?sym)
     [?e :model.node/symbol ?sym]
     [(missing? $ ?e :model.node/parent)]]
    ;; Bind k and v to attributes in node's meta
    [(node-meta ?node ?meta-k ?meta-v)
     [?node :model.node/meta ?meta]
     [?meta ?meta-k ?meta-v]]
    ;; Extract type of a node. Either check its meta tag
    ;; or check whether it's the root node and it is capitalized.
    [(node-type ?node ?type)
     (node-meta ?node :tag ?type)]
    [(node-type ?node ?type)
     [?node :model.node/symbol ?sym]
     (root-node ?node ?sym)
     [?node :model.node/capitalized? ?cap]
     [(true? ?cap)]
     [(identity ?sym) ?type]]])

(defn q
  "Query a model db with some predefined rules."
  [query-map model-db & inputs]
  {:pre [(= (count inputs)
            (dec (count (:in query-map '[$]))))]}
  (let [[inputs query] (query-with-rules query-map inputs query-rules)]
    (apply d/q query model-db inputs)))
