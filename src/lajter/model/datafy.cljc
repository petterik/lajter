(ns lajter.model.datafy
  (:require
    [clojure.core.protocols :as p]
    [datascript.core :as d]
    [lajter.model :as model]))

(declare nav-type nav-entity)

(defn- nav-fields [query model-db & query-args]
  (let [navigable-field
        (fn [field]
          (with-meta
            field
            {`p/nav (fn self [_ k v]
                      (cond
                        (= :type k) (nav-type model-db v)
                        (= :field k) (nav-entity model-db (d/entity model-db v))
                        (= :name k) v
                        (= :parent k) (nav-type model-db v)))}))

        fields
        (->> (apply model/q query model-db query-args)
             (map #(zipmap [:field :type :name :parent] %))
             (map (fn [{:keys [type name parent] :as field}]
                    (with-meta [parent type name]
                               {:nav (navigable-field field)})))
             (distinct))]
    (with-meta (sort fields)
               {`p/nav (fn [_ _ v]
                         (:nav (meta v)))})))

(def nav-fields-by-type
  (partial nav-fields
           '{:find  [?field ?field-type ?sym ?parent-type]
             :in    [$ ?parent-type]
             :where [(type-fields ?parent-type ?field)
                     (node-type ?field ?field-type)
                     [?field :model.node/symbol ?sym]]}))
(def nav-fields-of-type
  (partial nav-fields
           '{:find  [?field ?field-type ?sym ?parent-type]
             :in    [$ ?field-type]
             :where [(node-type ?field ?field-type)
                     [?field :model.node/symbol ?sym]
                     (type-fields ?parent-type ?field)]}))

(defn nav-type [model-db type]
  {:type    type
   :fields  (nav-fields-by-type model-db type)
   :used-by (nav-fields-of-type model-db type)})

(defn nav-entity [model-db entity]
  (with-meta
    (into {:db/id (:db/id entity)} entity)
    {`p/nav (fn [e k v]
              (condp = k
                :model.node/meta
                (nav-entity model-db v)
                :model.node/parent
                (nav-entity model-db v)
                :tag
                (nav-type model-db v)
                :model.node/symbol
                (if (:model.plugin.root-node/root? e)
                  (nav-type model-db v)
                  (nav-fields-by-type model-db v))
                ;; else
                (if-some [e (d/entity model-db v)]
                  (nav-entity model-db e)
                  v)))}))


(defn datafy
  ([model-db]
   (let [root-types
         (model/q '{:find  [?root ?type]
                    :where [(root-node ?root ?type)]}
                  model-db)]

     {:types    (with-meta
                  (sort (map second root-types))
                  {`p/nav (fn [_ _ type]
                            (nav-type model-db type))})
      :entities (sort-by
                  :db/id
                  (into []
                        (comp (map first)
                              (map (partial d/entity model-db))
                              (map #(select-keys % [:db/id :model.node/symbol]))
                              (map (partial nav-entity model-db)))
                        root-types))})))
