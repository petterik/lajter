(ns lajter.model.pull
  (:require
    [clojure.spec.alpha :as s]
    [datascript.core :as d]
    [lajter.model :as model]
    [lajter.model.db :as db]))

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

(defn pull [{:keys [model-db db]} pattern eid]
  (let [conformed (model/conform-with-parent-ids
                    model-db
                    ;; Pattern must be merged at this point.
                    ;; Wait, what's the point of going from
                    ;; merged pattern -> pattern, just to do pull?
                    ;; We should be able to do it from its db form.
                    ;; hmm.
                    ;; ^^^ Continue here.
                    pattern
                    #(throw (ex-info "Unable to find node in model-db" %)))

        field-type
        (fn [{:keys [parent sym]}]
          (model/q
            '{:find  [?type .]
              :in    [$ ?parent ?sym]
              :where [[?field :model.node/parent ?parent]
                      [?field :model.node/symbol ?sym]
                      (node-type ?field ?type)]}
            model-db
            parent
            sym))

        node->pattern
        (fn self [{:keys [sym selectors] :as node}]
          (let [k (keyword (name (field-type node)) (name sym))]
            (cond-> k
                    (seq selectors)
                    (hash-map (mapv self selectors)))))

        root->pattern
        (fn [{:keys [selectors]}]
          (mapv node->pattern selectors))]

    (into []
          (map (juxt :sym root->pattern))
          conformed)))