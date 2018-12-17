(ns lajter.model.pull
  (:require
    [clojure.spec.alpha :as s]
    [datascript.core :as d]
    [lajter.model :as model]
    [lajter.model.db :as db]))

(defn full-pattern
  "Given root symbol and a db with model indexed, extracts the
   full pull pattern for it, where the pattern conforms to ::model/model
   and not a datascript pattern."
  [model-db root-sym]
  (let [fields (model/q '{:find  [[?field ...]]
                          :in    [$ ?sym]
                          :where [(type-fields ?sym ?field)]}
                        model-db
                        root-sym)

        field->children
        (fn [field]
          (model/q '{:find  [[?fields ...]]
                     :in    [$ ?field]
                     :where [(node-type ?field ?ref-type)
                             (type-fields ?ref-type ?fields)]}
                   model-db
                   field))

        field->pattern
        (fn self [field]
          (let [field-name (:model.node/symbol (d/entity model-db field))
                fields (field->children field)]
            (cond-> [field-name]
                    (seq fields)
                    (conj (into [] (mapcat self) fields)))))]

    [root-sym (into [] (mapcat field->pattern) fields)]))

(defn keywordize-pattern
  "Takes a pattern and adds namespaced keywords to it such that
  it can be used with pull."
  [model-db pattern]
  (let [entity->field-types
        (fn [entity-type]
          (into {} (model/q '{:find  [?field-name ?field-type]
                              :in    [$ ?type]
                              :where [(type-fields ?type ?field)
                                      [?field :model.node/symbol ?field-name]
                                      (node-type ?field ?field-type)]}
                            model-db
                            entity-type)))
        ;; Takes a field and the type of the entity where it
        ;; comes from, returns either just the key or pull-pattern
        ;; join with the key and its selection.
        field->pattern
        (fn self [{:keys [field-types entity-type sym selectors]}]
          (let [field-name sym
                field-type (get field-types field-name)
                types (entity->field-types field-type)
                k (keyword (name entity-type) (name field-name))]
            (cond-> k
                    (seq selectors)
                    (hash-map
                      (into []
                            (comp
                              (map #(assoc % :entity-type field-type
                                             :field-types types))
                              (map self))
                            selectors)))))]

    (into []
          (comp
            (mapcat (fn [{:keys [sym selectors] :as root}]
                      (let [types (entity->field-types sym)]
                        (map (fn [node]
                               (assoc node :field-types types
                                           :entity-type sym))
                             selectors))))
            (map field->pattern))
          (s/conform ::model/model pattern))))

(defn pull
  "Pulls data from the data db in the shape of the pattern."
  [{:keys [model-db db]} pattern eid]
  (d/pull db (keywordize-pattern model-db pattern) eid))

(defn pull-many
  "Takes model and data db, a pattern conforming to ::model/model
  and entities to pull. Pulls the data from the data db in the
  shape of the pattern."
  [{:keys [model-db db]} pattern eids]
  (d/pull-many db (keywordize-pattern model-db pattern) eids))
