(ns lajter.model.db
  (:require
    [lajter.model :as model]
    [datascript.core :as d]))

(defmacro timing [label & body])

(defn datascript-schema
  "Given a db indexed with a model, returns datascript schema necessary
  to index and query data matching that model."
  [model-db]
  (let [field-key
        (memoize
          (fn [id]
            (let [e (d/entity model-db id)
                  ns (get-in e [:model.node/parent :model.node/symbol])]
              (keyword (name ns) (name (:model.node/symbol e))))))

        field-schema
        (fn [schema-map fields]
          (into {}
                (map (fn [id] [(field-key id) schema-map]))
                fields))

        ;; ref-types are types that have fields.
        ref-types
        (set
          (model/q '{:find  [[?type ...]]
                     :where [(type-fields ?type ?field)]}
                   model-db))

        refs
        (field-schema
          {:db/valueType :db.type/ref}
          (model/q '{:find  [[?field ...]]
                     :in    [$ ?ref-types]
                     :where [(node-type ?field ?type)
                             [(?ref-types ?type)]
                             (field ?field)]}
                   model-db
                   ref-types))

        many
        (field-schema
          {:db/cardinality :db.cardinality/many}
          (model/q '{:find  [[?field ...]]
                     :in    [$ [[?many-key ?many-val] ...]]
                     :where [(node-meta ?field ?many-key ?many-val)
                             (field ?field)]}
                   model-db
                   {:db.cardinality/many true
                    :db/cardinality      :db.cardinality/many}))

        indexed
        (field-schema
          {:db/index true}
          (model/q '{:find  [[?field ...]]
                     :where [(node-meta ?field :db/index true)
                             (field ?field)]}
                   model-db))

        components
        (field-schema
          {:db/isComponent true}
          (model/q '{:find  [[?field ...]]
                     :where [(node-meta ?field :db/isComponent true)
                             (field ?field)]}
                   model-db))

        unique
        (->> (model/q '{:find  [?field ?uniq]
                        :where [(node-meta ?field :db/unique ?uniq)
                                (field ?field)]}
                      model-db)
             (into {} (map (fn [[field uniq]]
                             [(field-key field) {:db/unique uniq}]))))]

    ;; Creates a map for each schema and field. Merges them at the end.
    ;; Refs
    ;; Unique
    ;; Many
    ;; db/index
    (merge-with merge (sorted-map) refs unique many indexed components)))

(defn with-model [db model-db data]
  (d/db-with db data))
