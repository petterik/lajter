(ns lajter.model
  "Namespace for indexing (merging) and querying domain
  models.

  The format of a domain model is that of Hodur:
  https://github.com/luchiniatwork/hodur-engine

  Examples:
    ;; Single Person entity.
  [Person]

  ;; Person entity with first and last name.
  [Person
   [first-name last-name]]

  ;; Symbols can have meta
  [^:root Person
   [^String full-name
    ^Person ^:many friends [full-name]]]

  Indexing is extensible. Add fields by passing
  :pipeline with optional :pipeline-opts to
  model/index-model. Your queries can then use
  this data.

  To make your custom index more powerful, add
  additional schema when creating the db.

  This namespace comes with rules which are useful
  with the default indexing. See model/query-rules
  and model/q, which uses the rules.

  Indexing with merge root nodes and their children
  by symbols being unique by their siblings.

  Metadata merging is customizable by passing
  :pipeline-opts {:merge-meta-fn (fn [env old new])}
  to model/index-model."
  (:require
    [clojure.spec.alpha :as s]
    [datascript.core :as d]))

(s/def ::model
  (s/* (s/cat :sym symbol?
              :selectors (s/? (s/and coll? ::model)))))

(def model-schema
  {:model.type/symbol {:db/unique :db.unique/identity}
   :model.node/symbol {:db/index true}
   :model.node/parent {:db/valueType :db.type/ref}
   :model.node/meta   {:db/valueType :db.type/ref}
   :tag               {:db/index true}})

(defn temp-id []
  (d/tempid :db.part/user))

(defn temp-id? [id]
  (neg? id))

(def plugin:root-node
  "Binds entity ?e to the root node with symbol ?sym"
  {:rule/clauses
   '[[(root-node ?e ?sym)
      [?e :model.node/symbol ?sym]
      [(missing? $ ?e :model.node/parent)]]]})

(def plugin:field
  "Coerces ?field to be a field, which are any non-root nodes."
  {:rule/clauses
   '[[(field ?field)
      [?field :model.node/parent _]]]})

(def plugin:node-meta
  "Bind k and v to attributes in node's meta"
  {:rule/clauses
   '[[(node-meta ?node ?meta-k ?meta-v)
      [?node :model.node/meta ?meta]
      [?meta ?meta-k ?meta-v]]]})

(def plugin:node-type
  "Extract type of a node. Either check its meta tag
  or check whether it's the root node and it is capitalized."
  {:pipeline/fn
   (let [capitalized?
         (fn [sym]
           (let [c (first (name sym))]
             #?(:clj  (Character/isUpperCase (char c))
                :cljs (= c (.toUpperCase c)))))]
     (fn [opts]
       (map (fn [node]
              (assoc node :model.node/capitalized?
                          (capitalized? (:model.node/symbol node)))))))

   :rule/clauses
   '[[(node-type ?node ?type)
      (node-meta ?node :tag ?type)]
     [(node-type ?node ?type)
      [?node :model.node/symbol ?sym]
      (root-node ?node ?sym)
      [?node :model.node/capitalized? ?cap]
      [(true? ?cap)]
      [(identity ?sym) ?type]]]})

(def plugin:type-fields
  "Get all the fields for a symbol.
  The fields are the root node selector
  or the fields tagged with the symbol's selector."
  {:rule/clauses
   '[[(type-fields ?sym ?field)
      (root-node ?root ?sym)
      [?field :model.node/parent ?root]]
     [(type-fields ?sym ?field)
      [?meta :tag ?sym]
      [?tagged :model.node/meta ?meta]
      [?field :model.node/parent ?tagged]]]})

(defn find-meta
  "Returns db/id of meta data entity for a node's db/id."
  [meta-db node-id]
  (-> (d/entity meta-db node-id)
      (:model.node/meta)
      (:db/id)))

(defn- default-merge-meta-fn
  "Default implementation of handling merge conflicts when
  merging meta data for a node. Returning the new value if
  not nil."
  [env old-value new-value]
  (or new-value old-value))

(def plugin:merge-meta
  {:pipeline/fn
   (fn [{:keys [db merge-meta-fn]
         :or   {merge-meta-fn default-merge-meta-fn}
         :as   pipeline-opts}]
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
                             (assoc new-meta :db/id meta-id)))))))})

(def default-plugins
  "Plugins added to all model databases. Plugins are used to extend
  and give meaning to positioning and metadata of the model.

  Plugins consist of :db/schema :rule/clauses and :pipeline/fn.
  schema and rule clauses are used for query and transacting of model.
  pipeline/fn is called as nodes are being transacted to the model db.
  "
  [plugin:root-node
   plugin:field
   plugin:node-meta
   plugin:node-type
   plugin:type-fields
   plugin:merge-meta])

(defn impl-entity
  "Returns the implementation detail entity containing rules and pipelines"
  [meta-db]
  (d/entity meta-db [:model.impl/id 1]))

(defn- update-impl [meta-db k f & args]
  (let [impl (impl-entity meta-db)
        v (get impl k)]
    (d/db-with meta-db [[:db/add (:db/id impl) k (apply f v args)]])))

(defn db-with-plugins
  "Returns a db with plugins added to it."
  ([meta-db plugins]
   (let [schema (transduce (map :db/schema)
                           (partial merge-with merge)
                           (:schema meta-db)
                           plugins)
         db (cond
              (nil? meta-db) (d/init-db [] schema)
              (seq schema) (d/init-db (d/datoms meta-db :eavt) schema)
              :else meta-db)]
     (-> db
         (update-impl :model.impl/rules into (mapcat :rule/clauses) plugins)
         (update-impl :model.impl/pipeline into (keep :pipeline/fn) plugins)))))

(defn init-meta-db
  ([] (init-meta-db nil))
  ([plugins]
   (let [db (-> (d/empty-db {:model.impl/id {:db/unique :db.unique/value}})
                (d/db-with [{:model.impl/id       1
                             :model.impl/rules    []
                             :model.impl/pipeline []}]))]
     (db-with-plugins db (concat [{:db/schema model-schema}]
                                 default-plugins
                                 plugins)))))

(declare q)

(defn find-root
  "Returns db/id of root node for a symbol."
  [meta-db sym]
  ;; Hand rolling this query makes indexing about 2x faster
  #_(q '{:find  [?e .]
         :in    [$ ?sym]
         :where [(root-node ?e ?sym)]}
       meta-db
       sym)
  (first
    (eduction
      (map :e)
      (filter (fn missing? [e]
                (empty?
                  (d/datoms meta-db :eavt e :model.node/parent))))
      (take 1)
      (d/datoms meta-db :avet :model.node/symbol sym))))

(defn find-node-by-parent
  "Returns db/id of root node for a symbol and parent."
  [meta-db sym parent]
  (when-not (temp-id? parent)
    (d/q '{:find  [?e .]
           :in    [$ ?sym ?parent]
           :where [[?e :model.node/parent ?parent]
                   [?e :model.node/symbol ?sym]]}
         meta-db
         sym
         parent)))

(defn- selectors-with-parent-id
  "Takes a conformed model root and a db, updates the root's selectors
  to include db/id's of its parents.

  Calls the :missing-id-fn with 1 arg when the node was not found in
  the db."
  [meta-db root]
  (let [add-node-id
        (fn self [parent {:keys [sym selectors] :as m}]
          (let [id (or (find-node-by-parent meta-db sym parent)
                       (temp-id))]
            (cons (assoc m :id id :parent parent)
                  (mapcat #(self id %)
                          selectors))))

        id (or (find-root meta-db (:sym root))
               (temp-id))]
    (-> root
        (assoc :id id)
        (update :selectors (partial mapcat #(add-node-id id %))))))

(defn root->node-seq
  "Takes a conformed model root and returns a seq of model nodes to
  be transacted in to a db."
  [root]
  (map (fn [{:keys [id sym parent]}]
         (let [m (meta sym)]
           (cond-> {:db/id             id
                    :model.node/symbol sym}
                   (seq m)
                   (assoc :model.node/meta m)
                   (some? parent)
                   (assoc :model.node/parent parent))))
       (cons root (:selectors root))))

(defn index-model
  "Takes a meta-db and a model conforming to ::model.
  Returns a new meta-db with the model indexed and merged with
  the existing model.

  Merge rules:
  * Roots are unique by their symbol.
  * A node or root cannot have two children with the same symbol."
  ([meta-db model]
   (index-model meta-db model {}))
  ([meta-db model {:keys [pipeline-opts]}]
   (let [impl (impl-entity meta-db)
         pipeline-fn (fn [opts]
                       (map #(% opts) (:model.impl/pipeline impl)))]
     (reduce
       (fn [meta-db root]
         (let [xf (apply comp (pipeline-fn (assoc pipeline-opts :db meta-db)))]
           (->> root
                (selectors-with-parent-id meta-db)
                (root->node-seq)
                (into [] xf)
                (d/db-with meta-db))))
       meta-db
       (s/conform ::model model)))))

(defn- find-index
  "Finds the first index where pred is truthy in coll."
  [pred coll]
  (->> coll
       (keep-indexed (fn [idx x] (when (pred x) idx)))
       (first)))

(defn- query-with-rules
  "Given query, inputs to datascript.core/q and query rules,
  adds the rule symbol (%) to the query unless it's in there,
  and concatenate the given rules to the inputs.

  Returns the input and query with rules added to them."
  [query-map inputs rules]
  (let [inputs (vec inputs)
        rule-idx (find-index #{'%} (rest (:in query-map)))
        inputs (if rule-idx
                 (update inputs rule-idx (partial into rules))
                 (into inputs [rules]))
        in (cond-> (or (some-> (:in query-map) (not-empty) (vec))
                       '[$])
                   (not rule-idx)
                   (into '[%]))]
    [inputs (assoc query-map :in (vec in))]))

(defn q
  "Query a model db with some predefined rules."
  [query-map model-db & inputs]
  {:pre [(= (count inputs)
            (dec (count (:in query-map '[$]))))]}
  (let [query-rules (:model.impl/rules (impl-entity model-db))
        [inputs query] (query-with-rules query-map inputs query-rules)]
    (apply d/q query model-db inputs)))
