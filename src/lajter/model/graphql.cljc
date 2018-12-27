(ns lajter.model.graphql
  (:require
    [clojure.spec.alpha :as s]
    [clojure.set :as set]
    [lajter.model :as model]))

;; schema spec generated by spec-provider
(s/def :gql/name symbol?)
(s/def :gql/kind '#{UNION ENUM LIST OBJECT INPUT_OBJECT
                    NON_NULL SCALAR INTERFACE})

(s/def :gql/ofType (s/keys :req-un [:gql/kind]
                           :opt-un [:gql/name :gql/ofType]))
(s/def :gql/type (s/keys :req-un [:gql/kind]
                         :opt-un [:gql/name :gql/ofType]))
(s/def :gql/args (s/coll-of (s/keys :req-un [:gql/type])))

(s/def :gql/fields (s/coll-of (s/keys :req-un [:gql/name :gql/type]
                                      :opt-un [:gql/args])))
(s/def :gql/types (s/coll-of (s/keys :req-un [:gql/kind :gql/name]
                                     :opt-un [:gql/fields])))

(defn schema->model
  "Takes a graphql schema query (defined in resources/.../github-schema.gql)
  keywordized and symbolized to match the :gql/types spec. Returns a model
  matching spec :lajter.model/model.

  Metadata on the field names include:
  * When the type is required
  * Cardinality of the type
  * Whether cardinalty many fields cannot be nil (has to be empty list).
  * Input fields with names and type information."
  ([]
   (let [type->meta
         (fn self [{:keys [name kind ofType] ::keys [req?] :as node}]
           (condp = kind
             'NON_NULL (self (assoc ofType ::req? true))
             'LIST (merge {:gql.type/list?          true
                           :gql.type/required-list? req?}
                          (self ofType))
             ;; else
             (if (seq ofType)
               (throw (ex-info "Unexpected type node was 'ofType'"
                               {:node node}))
               (cond-> {:gql.type/kind            kind
                        :gql.type/name            name
                        :gql.type/required-value? req?}))))

         field->model
         (fn [{:keys [name args type]}]
           (let [input-args (mapv (fn [{:keys [name type]}]
                                    {:gql.field.input/name name
                                     :gql.field.input/type (type->meta type)})
                                  args)]
             (with-meta name {:gql.field/type   (type->meta type)
                              :gql.field/inputs input-args})))

         root-type->model
         (fn [{:keys [name kind fields possibleTypes]}]
           (let [met (hash-map
                       :gql.root/type {:gql.type/name name
                                       :gql.type/kind kind}
                       :gql.root/possible-types
                       (map #(set/rename-keys % {:name :gql.type/name
                                                 :kind :gql.type/kind})
                            possibleTypes))]
             (cond-> [(with-meta name met)]
                     (seq fields)
                     (conj (mapv field->model fields)))))]
     (mapcat root-type->model)))
  ([graphql-types]
   (into [] (schema->model) graphql-types)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plugin

(defn- field-input->field-node
  "Takes a field input model meta and returns a input field node
  that is to be indexed."
  [db field-node field-input]
  (let [old-id (model/q '{:find  [?e .]
                          :in    [$ ?field ?input-name]
                          :where [[?e :gql.field.input/field ?field]
                                  [?e :gql.field.input/name ?input-name]]}
                        db
                        (:db/id field-node)
                        (:gql.field.input/name field-input))]

    (->> (assoc field-input :gql.field.input/field (:db/id field-node))
         (model/merge-node db old-id))))

(def plugin:graphql-root-cleanup
  {:pipeline/fn
   (fn [_]
     (map (fn [node]
            (let [{:model.node/keys [parent meta]} node
                  {:gql.root/keys [possible-types]} meta
                  root? (not parent)]
              (cond-> node
                      (and root?
                           ;; TODO: Do something with possibleTypes
                           ;;       when kind is UNION or INTERFACE
                           (empty? possible-types))
                      (update :model.node/meta
                              dissoc
                              :gql.root/possible-types))))))})

(def plugin:graphql-input-fields
  {:pipeline/fn
   (fn [{:keys [db]}]
     (mapcat
       (fn [node]
         (let [->field-node #(field-input->field-node db node %)
               ;; Always remove field inputs as we create a dedicated node for
               ;; each of them.
               node (update node :model.node/meta dissoc :gql.field/inputs)]
           (cons node (map ->field-node (:gql.field/inputs node)))))))})

(def plugin:graphql-types
  {:db/schema
   {:gql.type/required {:db/valueType :db.type/ref}}
   :pipeline/fn
   (fn [opts]
     (map (fn [node]
            (let [met (:model.node/meta node)
                  gql-type (or (:gql.root/type met)
                               (:gql.field/type met)
                               (:gql.field.input/type node))

                  req-map (into {}
                                (filter (comp some? val))
                                (select-keys gql-type [:gql.type/required-list?
                                                       :gql.type/required-value?]))

                  {:gql.type/keys [name list?]} gql-type
                  new-met (cond-> met
                                  (some? name)
                                  (assoc :tag name)
                                  (some? list?)
                                  (assoc :db/cardinality :db.cardinality/many)
                                  (seq req-map)
                                  (assoc :gql.type/required req-map)
                                  :always
                                  (dissoc :gql.root/type :gql.field/type))
                  node (-> node
                           (dissoc :gql.field.input/type)
                           (cond-> (:tag new-met)
                                   (dissoc :model.plugin.node-type/capitalized?)))]

              (assoc node :model.node/meta new-met)))))})


(def plugin:graphql
  (model/merge-plugins plugin:graphql-input-fields
                       plugin:graphql-root-cleanup
                       plugin:graphql-types))
