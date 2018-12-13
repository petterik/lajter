(ns lajter.model.db
  (:require
    [lajter.model :as model]
    [datascript.core :as d]))

(def ref-type-rule
  "Returns all the reference type symbols. Basically all the
  types that is defined by the model."
  '[[(ref-types ?sym)
     [?field :model.node/parent ?parent]
     (root-node ?parent ?sym)]
    ;; For inline definitions of types:
    ;; TODO: Does this complicate things too much?
    ;;       Maybe we should take this out of the "spec"?
    [(ref-types ?sym)
     [?field :model.node/parent ?parent]
     [?parent :model.node/parent _]
     [?parent :model.node/meta ?meta]
     [?meta :tag ?sym]]])

(def unique-rule
  '[[(unique ?field ?uniq)
     (node-meta ?field :db/unique ?uniq)]
    [(unique ?field ?uniq)
     [(identity [UUID ID]) [?unique-type ...]]
     (node-meta ?field :tag ?unique-type)
     [(identity :db.unique/identity) ?uniq]]])

(defn datascript-schema
  "Given a db indexed with a model, returns datascript schema necessary
  to index and query data matching that model."
  [meta-db]
  (let [field-key
        (memoize
          (fn [id]
            (let [e (d/entity meta-db id)
                  ns (get-in e [:model.node/parent
                                :model.node/symbol])]
              (keyword (name ns) (name (:model.node/symbol e))))))

        ref-map {:db/valueType :db.type/ref}
        refs (->> (model/q '{:find  [[?field ...]]
                             :in    [$ %]
                             :where [(ref-types ?sym)
                                     [?meta :tag ?sym]
                                     [?field :model.node/meta ?meta]
                                     (field ?field)]}
                           meta-db
                           ref-type-rule)
                  (into {} (map (fn [id]
                                  [(field-key id) ref-map]))))

        unique (->> (model/q '{:find  [?field ?uniq]
                               :in    [$ %]
                               :where [(field ?field)
                                       (unique ?field ?uniq)]}
                             meta-db
                             unique-rule)
                    (into {} (map (fn [[field uniq]]
                                    [(field-key field) {:db/unique uniq}]))))

        card-map {:db/cardinality :db.cardinality/many}
        many (->> (model/q '{:find  [[?field ...]]
                             :in    [$ [[?many-key ?many-val] ...]]
                             :where [(field ?field)
                                     (node-meta ?field ?many-key ?many-val)]}
                           meta-db
                           [[:many true]
                            [:db/cardinality :db.cardinality/many]])
                  (into {} (map (fn [id]
                                  [(field-key id) card-map]))))

        idx-map {:db/index true}
        index (->> (model/q '{:find  [[?field ...]]
                              :where [(field ?field)
                                      (node-meta ?field :db/index true)]}
                            meta-db)
                   (into {} (map (fn [id]
                                   [(field-key id) idx-map]))))]

    ;; Creates a map for each schema and field. Merges them at the end.
    ;; Refs
    ;; Unique
    ;; Many
    ;; db/index
    (merge-with merge (sorted-map) refs unique many index)))