(ns lajter.model.pull
  (:require
    [datascript.core :as d]
    [lajter.model :as model]))

(defn full-pattern
  "Extracts the full pull pattern for a given root symbol."
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


