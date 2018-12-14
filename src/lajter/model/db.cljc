(ns lajter.model.db
  (:require
    [lajter.model :as model]
    [datascript.core :as d]))

;; TODO: Could be done with just an OR clause and DataScript 0.17.1
(def unique-rule
  '[[(unique ?field ?uniq)
     (node-meta ?field :db/unique ?uniq)]
    [(unique ?field ?uniq)
     [(identity [UUID ID]) [?unique-type ...]]
     (node-meta ?field :tag ?unique-type)
     [(identity :db.unique/identity) ?uniq]]])

;; TODO: What is this doing here?
(defn field-schema [schema-map fields]
  )

(defn datascript-schema
  "Given a db indexed with a model, returns datascript schema necessary
  to index and query data matching that model."
  [model-db]
  (let [field-key
        (memoize
          (fn [id]
            (let [e (d/entity model-db id)
                  ns (get-in e [:model.node/parent
                                :model.node/symbol])]
              (keyword (name ns) (name (:model.node/symbol e))))))

        field-schema
        (fn [schema-map fields]
          (into {}
                (map (fn [id] [(field-key id) schema-map]))
                fields))

        refs
        (field-schema
          {:db/valueType :db.type/ref}
          (model/q '{:find  [[?field ...]]
                     :where [(type-fields _ ?field)]}
                   model-db))

        many
        (field-schema
          {:db/cardinality :db.cardinality/many}
          (model/q '{:find  [[?field ...]]
                     :in    [$ [[?many-key ?many-val] ...]]
                     :where [(field ?field)
                             (node-meta ?field ?many-key ?many-val)]}
                   model-db
                   [[:many true]
                    [:db/cardinality :db.cardinality/many]]))

        indexed
        (field-schema
          {:db/index true}
          (model/q '{:find  [[?field ...]]
                     :where [(field ?field)
                             (node-meta ?field :db/index true)]}
                   model-db))

        components
        (field-schema
          {:db/isComponent true}
          (model/q
            '{:find  [[?field ...]]
              :in    [$ [?component-key ...]]
              :where [[?meta ?component-key true]
                      [?field :model.node/meta ?meta]]}
            model-db
            [:component :db/isComponent]))

        unique
        (->> (model/q '{:find  [?field ?uniq]
                        :in    [$ %]
                        :where [(field ?field)
                                (unique ?field ?uniq)]}
                      model-db
                      unique-rule)
             (into {} (map (fn [[field uniq]]
                             [(field-key field) {:db/unique uniq}]))))]

    ;; Creates a map for each schema and field. Merges them at the end.
    ;; Refs
    ;; Unique
    ;; Many
    ;; db/index
    (merge-with merge (sorted-map) refs unique many indexed components)))

(defn with-model [db model-db data]
  (d/db-with db (sequence cat data)))
