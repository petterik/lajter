(ns lajter.model.db
  (:require
    [lajter.model :as model]
    [datascript.core :as d]))

(defn datascript-schema
  "Given a db indexed with a model, returns datascript schema necessary
  to index and query data matching that model."
  [model-db]
  (let [field-entity
        (memoize
          (fn [id]
            (let [e (d/entity model-db id)]
              (when (:model.node/parent e)
                e))))

        field-key
        (fn [e]
          (let [ns (get-in e [:model.node/parent :model.node/symbol])]
            (keyword (name ns) (name (:model.node/symbol e)))))

        field-schema
        (fn [schema-map fields]
          (into {}
                (comp (keep field-entity)
                      (map (fn [e] [(field-key e) schema-map])))
                fields))

        ;; ref-types are types that have fields.
        ref-types
        (model/q
          '{:find  [[?type ...]]
            :where [(type-fields ?type _)]}
          model-db)

        refs
        (field-schema
          {:db/valueType :db.type/ref}
          (model/q '{:find  [[?field ...]]
                     :in    [$ [?type ...]]
                     :where [(node-type ?field ?type)]}
                   model-db
                   ref-types))

        many
        (field-schema
          {:db/cardinality :db.cardinality/many}
          (model/q '{:find  [[?field ...]]
                     :where [(or [?meta :db.cardinality/many true]
                                 [?meta :db/cardinality :db.cardinality/many])
                             [?field :model.node/meta ?meta]]}
                   model-db))

        indexed
        (field-schema
          {:db/index true}
          (model/q '{:find  [[?field ...]]
                     :where [(node-meta ?field :db/index true)]}
                   model-db))

        components
        (field-schema
          {:db/isComponent true}
          (model/q '{:find  [[?field ...]]
                     :where [(node-meta ?field :db/isComponent true)]}
                   model-db))

        unique
        (->> (model/q '{:find  [?field ?uniq]
                        :where [(node-meta ?field :db/unique ?uniq)]}
                      model-db)
             (into {}
                   (keep (fn [[field uniq]]
                           (when-let [e (field-entity field)]
                             [(field-key e) {:db/unique uniq}])))))]

    ;; Creates a map for each schema and field. Merges them at the end.
    ;; Refs
    ;; Unique
    ;; Many
    ;; db/index
    (merge-with merge (sorted-map) refs unique many indexed components)))

(defn with-model [db model-db data]
  (d/db-with db data))
