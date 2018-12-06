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
  '[Model.Type
    [^{:db/unique :db.unique/identity} symbol]
    Model.Entity
    [^:db/index symbol
     ^Model.Type type]
    ^{:model/interface Model.Entity} Model.Field
    [^:db/index symbol
     ^Model.Type type
     ^Model.Entity parent]])

(def primitive-type? #(contains? primitive-types %))
(def reference-type? (every-pred some?
                                 (complement primitive-type?)))

(defn lower-case [sym]
  (string/lower-case (name sym)))

(defn model->datascript-schema [model]
  (let [selector->schema
        (fn [{:keys [sym selectors]}]
          (let [{:keys [tag] :as meta} (meta sym)]
            (cond-> (m/filter-keys (comp #{"db"} namespace)
                                   meta)
                    (or (reference-type? tag)
                        (seq selectors))
                    (assoc :db/valueType :db.type/ref))))]
    (into {}
          (mapcat (fn [{:keys [sym selectors]}]
                    (let [field-key (partial keyword (lower-case sym))]
                      (map (juxt (comp field-key lower-case :sym)
                                 selector->schema)
                           selectors))))
          (s/conform ::model model))))

(def model-schema (model->datascript-schema model-schema-model))

(defn meta-db []
  (d/db (d/create-conn model-schema)))

(def root-entity-rule
  '[[(root-node ?e ?sym)
     [?e :model.entity/symbol ?sym]
     [(missing $ ?e :model.field/parent)]]
    [(meta-matching)]])

(defn update-when [m k f & args]
  (if (some? (get m k))

    m))

(defn selector->tx [{:keys [parent sym selectors]}]
  ;; TODO: This won't work:
  #_(let [entity-id (d/tempid :db.part/user)
        field-id (d/tempid :db.part/user)
        field? (some? parent)
        {:keys [tag] :as mdata} (meta sym)]
    (if field?
      (cond-> {:db/id              id
               :model.field/parent parent
               :model.field/symbol sym}
              (some? tag)
              (assoc :model.field/type [:model.type/symbol tag])
              (seq selectors)
              (vector ))))
  ;; We need to merge new selectors in to the index
  ;; based on what's already merged.
  ;; We need to walk the db with each selector, passing
  ;; parent and looking up the field by sym at each step.
  ;; I think this might be useful for later stuff as well.
  ;; TODO: Need to pass in DB to this function.
  )



(defn index-model [meta-db model]
  (->> (s/conform ::model model)
       (eduction
         (mapcat (fn self [{:keys [parent sym selectors]}]
                   (let [id (d/tempid :db.part/user)]
                     (cons (cond-> {:db/id               id
                                    :model.entity/symbol sym}
                                   (some? parent)
                                   (assoc :model.field/parent parent))
                           (eduction
                             (map #(assoc % :parent id))
                             (map self)
                             selectors))))))
       (d/db-with meta-db)))
