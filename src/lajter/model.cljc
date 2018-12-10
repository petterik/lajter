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

(def model-schema-model
  '[Model.Meta

    Model.Type
    [^{:db/unique :db.unique/identity} symbol]

    Model.Node
    [^:db/index symbol
     ^Model.Node parent
     ^Model.Meta meta]])

(def primitive-type? #(contains? primitive-types %))
(def reference-type? (every-pred some?
                                 (complement primitive-type?)))

(defn lower-case [sym]
  (string/lower-case (name sym)))

(defn model->datascript-schema [model]
  (let [field->namespaced-key
        (fn [{:keys [sym parent-sym]}]
          (keyword (lower-case parent-sym) (lower-case sym)))

        field->schema
        (fn [{:keys [sym selectors]}]
          (let [{:keys [tag] :as met} (meta sym)]
            (-> (m/filter-keys (comp #{"db"} namespace)
                               met)
                (cond-> (or (reference-type? tag)
                            (seq selectors))
                        (assoc :db/valueType :db.type/ref)))))]
    (into {}
          (mapcat (fn [{:keys [sym selectors]}]
                    (eduction
                      (map #(assoc % :parent-sym sym))
                      (map (juxt field->namespaced-key
                                 field->schema))
                      selectors)))
          (s/conform ::model model))))

(def model-schema (model->datascript-schema model-schema-model))
(def type-entities (map #(hash-map :model.type/symbol %) primitive-types))

(defn init-meta-db
  ([] (init-meta-db nil))
  ([schema]
   (-> (d/create-conn (merge-with merge model-schema schema))
       (d/db)
       (d/db-with type-entities))))

(defn is-upper-case? [c]
  #?(:clj  (Character/isUpperCase (char c))
     :cljs (= (str c) (string/upper-case (str c)))))

(defn- temp-id [] (d/tempid :db.part/user))
(defn- temp-id? [id]
  (neg? id))

(declare q)

(defn find-root [meta-db sym]
  (q '{:find  [?e .]
         :in    [$ ?sym]
         :where [(root-node ?e ?sym)]}
       meta-db
       sym))

(defn find-node [meta-db parent sym]
  (q '{:find  [?e .]
         :in    [$ ?parent ?sym]
         :where [[?e :model.node/parent ?parent]
                 [?e :model.node/symbol ?sym]]}
       meta-db
       parent
       sym))

(defn- memoize-found-id
  "Takes a function which tries to find an existing db id
  for a node. Returns a function which, given the same node
  input (such as sym, or sym & parent), returns the same id."
  [find-id-fn]
  (memoize (comp #(or % (temp-id)) (partial find-id-fn))))

(defn flatten-model-xf [meta-db]
  (let [find-root (memoize-found-id (partial find-root meta-db))
        find-node (memoize-found-id (partial find-node meta-db))
        add-node-id
        (fn self [{:keys [sym parent] :as m}]
          (let [id (find-node sym parent)]
            (-> (assoc m :id id)
                (update :selectors (partial map #(self (assoc % :parent id)))))))]
    (mapcat (fn [{:keys [sym selectors] :as m}]
              (let [id (find-root sym)]
                (cons (assoc m :id id)
                      (map (comp add-node-id #(assoc % :parent id))
                           selectors)))))))

(defn default-pipeline
  "Pipeline taking a flattened model, linked by db ids and
  returns nodes to transacted in to the model database.

  The idea is that we can use this pipeline to hook in to
  whatever we might need in the future as far as merging
  data goes."
  [meta-db]
  [(map (fn [{:keys [id sym]}]
          (let [m (meta sym)]
            (cond-> {:db/id             id
                     :model.node/symbol sym}
                    (seq m)
                    (assoc :model.node/meta m)))))
   ;; needs is-capitalized attribute to figure out whether
   ;; the node is its own type definition or not.
   (let [is-capitalized?
         (fn [sym]
           (let [c (first (name sym))]
             #?(:clj  (Character/isUpperCase (char c))
                :cljs (= c (.toUpperCase c)))))]
     (map #(assoc % :model.node/is-capitalized?
                    (is-capitalized? (:model.node/symbol %)))))

   ;; TODO: Merge metadata.
   ])

(defn index-model
  ([meta-db model]
   (index-model meta-db model {:pipeline default-pipeline}))
  ([meta-db model {:keys [pipeline]}]
   (->> (s/conform ::model model)
        (into [] (apply comp
                        (flatten-model-xf meta-db)
                        (pipeline meta-db)))
        (d/db-with meta-db))))


(defn- query-with-rules [query-map inputs rules]
  (let [in (:in query-map)
        has-rules? (some #{'%} in)
        inputs (if has-rules?
                 (update (vec inputs) 0 into rules)
                 (cons rules inputs))
        in (cond
             has-rules? in
             (seq in) (cons (first in) (cons '% (rest in)))
             :else '[$ %])]
    [inputs (assoc query-map :in in)]))

(def query-rules
  ['[(root-node ?node ?sym)
     [(missing? $ ?node :model.node/parent)]
     [?node :model.node/symbol ?sym]]
   ;; Bind k and v to attributes in node's meta
   '[(node-meta ?node ?meta-k ?meta-v)
     [?node :model.node/meta ?meta]
     [?meta ?meta-k ?meta-v]]
   ;; Extract type of a node. Either check its meta tag
   ;; or check whether it's the root node and it is capitalized.
   '[(node-type ?node ?type)
     (node-meta ?node :tag ?type)]
   '[(node-type ?node ?type)
     [?node :model.node/symbol ?sym]
     (root-node ?node ?sym)
     [?node :model.node/is-capitalized? ?cap]
     [(true? ?cap)]
     [(identity ?sym) ?type]]])

(defn q
  "Query a model db with some predefined rules."
  [query-map model-db & inputs]
  {:pre [(= (count inputs)
            (dec (count (:in query-map '[$]))))]}
  (let [[inputs query] (query-with-rules query-map inputs query-rules)]
    (apply d/q query model-db inputs)))
