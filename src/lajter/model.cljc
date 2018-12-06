(ns lajter.model
  (:require
    [clojure.set :as set]
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
    Model.Field
    [^:db/index symbol
     ^Model.Entity entity
     ^Model.Type type]])

(s/conform ::model model-schema-model)

(defn lower-case [sym]
  (string/lower-case (name sym)))

(defn model->datascript-schema [model]
  (let [selector->schema
        (fn [{:keys [sym]}]
          (let [{:keys [tag] :as meta} (meta sym)]
            (cond-> (m/filter-keys (comp #{"db"} namespace)
                                   meta)
                    (some? tag)
                    (assoc :db/valueType :db.type/ref))))]
    (into {}
          (mapcat (fn [{:keys [sym selectors]}]
                    (let [field-key (partial keyword (lower-case sym))]
                      (map (juxt (comp field-key lower-case :sym)
                                selector->schema)
                          selectors))))
          (s/conform ::model model))))

