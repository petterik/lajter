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
     ^Model.Meta meta
     ^Model.Type type]])

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

(defn init-meta-db []
  (-> (d/create-conn model-schema)
      (d/db)
      (d/db-with (map #(hash-map :model.type/symbol %) primitive-types))))

(def root-entity-rule
  '[[(root-node ?node ?sym)
     [?node :model.node/symbol ?sym]
     [(missing? $ ?node :model.node/parent)]]
    [(meta-matching ?node ?meta-k ?meta-v)
     [?node :model.node/meta ?meta]
     [?meta ?meta-k ?meta-v]]])

;; We need to merge new selectors in to the index
;; based on what's already merged.
;; We need to walk the db with each selector, passing
;; parent and looking up the field by sym at each step.
;; I think this might be useful for later stuff as well.
;; TODO: Need to pass in DB to this function.

(defn- temp-id [] (d/tempid :db.part/user))
(defn- temp-id? [id]
  (neg? id))

(defn find-root [meta-db sym]
  (d/q '{:find  [?e .]
         :in    [$ % ?sym]
         :where [(root-node ?e ?sym)]}
       meta-db
       root-entity-rule
       sym))

(defn find-node [meta-db parent sym]
  (d/q '{:find  [?e .]
         :in    [$ % ?parent ?sym]
         :where [[?e :model.node/parent ?parent]
                 [?e :model.node/symbol ?sym]]}
       meta-db
       root-entity-rule
       parent
       sym))

(defn find-type [meta-db tag]
  (:db/id (d/entity meta-db [:model.type/symbol tag])))

(defn- memoize-found-id
  "Takes a function which tries to find an existing db id
  for a node. Returns a function which, given the same node
  input (such as sym, or sym & parent), returns the same id."
  [find-id-fn]
  (memoize (comp #(or % (temp-id)) (partial find-id-fn))))

(defn index-model [meta-db model]
  (let [find-root (memoize-found-id (partial find-root meta-db))
        find-node (memoize-found-id (partial find-node meta-db))
        find-type (memoize-found-id (partial find-type meta-db))

        type-map (fn [sym]
                   {:db/id             (find-type sym)
                    :model.type/symbol sym})

        add-parent
        (fn self [selectors parent]
          (->> selectors
               (map #(assoc % :parent parent
                              :db/id (find-node parent (:sym %))
                              :meta (meta (:sym %))))
               (map #(update % :selectors self (:db/id %)))))

        node->tx
        (fn self [{:keys [sym selectors db/id meta parent]}]
          (let [tag (:tag meta)
                node (cond-> {:db/id             id
                              :model.node/parent parent
                              :model.node/symbol sym}
                             (seq meta)
                             (assoc :model.node/meta meta)
                             tag
                             (assoc :model.node/type (type-map tag)))]
            (cons node (mapcat self selectors))))]

    (->> (s/conform ::model model)
         (map (fn [{:keys [sym] :as root}]
                (let [db-id (find-root sym)]
                  (-> (assoc root :db/id db-id
                                  :meta (meta sym))
                      (update :selectors add-parent db-id)))))
         (mapcat
           (fn [{:keys [sym meta db/id selectors]}]
             (let [node (cond-> {:db/id             id
                                 :model.node/symbol sym
                                 ;; Root nodes are their own types.
                                 :model.node/type   (type-map sym)}
                                (seq meta)
                                (assoc :model.node/meta meta))]
               (cons node (mapcat node->tx selectors)))))
         (d/db-with meta-db))))
