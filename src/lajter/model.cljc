(ns lajter.model
  (:require
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
  '[Model.Meta

    Model.Type
    [^{:db/unique :db.unique/identity} symbol]

    Model.Node
    [^:db/index symbol
     ^Model.Node parent
     ^Model.Meta meta
     ^Model.Type type]])

(def primitive-type? #(contains? primitive-types %))
(def reference-type? (every-pred some?
                                 (complement primitive-type?)))

(defn lower-case [sym]
  (string/lower-case (name sym)))

(defn model->datascript-schema [model]
  (let [field->namespaced-key
        (fn [{:keys [sym parent-sym]}]
          (keyword (lower-case parent-sym) (lower-case sym)))

        field->schema
        (fn [{:keys [sym selectors]}]
          (let [{:keys [tag] :as met} (meta sym)]
            (-> (m/filter-keys (comp #{"db"} namespace)
                               met)
                (cond-> (or (reference-type? tag)
                            (seq selectors))
                        (assoc :db/valueType :db.type/ref)))))]
    (into {}
          (mapcat (fn [{:keys [sym selectors]}]
                    (eduction
                      (map #(assoc % :parent-sym sym))
                      (map (juxt field->namespaced-key
                                 field->schema))
                      selectors)))
          (s/conform ::model model))))

(def model-schema (model->datascript-schema model-schema-model))

(defn init-meta-db []
  (-> (d/create-conn model-schema)
      (d/db)
      (d/db-with (map #(hash-map :model.type/symbol %) primitive-types))))

(defn is-upper-case? [c]
  #?(:clj  (Character/isUpperCase (char c))
     :cljs (= (str c) (string/upper-case (str c)))))

;; We need to merge new selectors in to the index
;; based on what's already merged.
;; We need to walk the db with each selector, passing
;; parent and looking up the field by sym at each step.
;; I think this might be useful for later stuff as well.
;; TODO: Need to pass in DB to this function.

(defn- temp-id [] (d/tempid :db.part/user))
(defn- temp-id? [id]
  (neg? id))

(declare q)

(defn find-root [meta-db sym]
  (q '{:find  [?e .]
         :in    [$ ?sym]
         :where [(root-node ?e ?sym)]}
       meta-db
       sym))

(defn find-node [meta-db parent sym]
  (q '{:find  [?e .]
         :in    [$ ?parent ?sym]
         :where [[?e :model.node/parent ?parent]
                 [?e :model.node/symbol ?sym]]}
       meta-db
       parent
       sym))

(defn find-type [meta-db tag]
  (:db/id (d/entity meta-db [:model.type/symbol tag])))

(defn- memoize-found-id
  "Takes a function which tries to find an existing db id
  for a node. Returns a function which, given the same node
  input (such as sym, or sym & parent), returns the same id."
  [find-id-fn]
  (memoize (comp #(or % (temp-id)) (partial find-id-fn))))

(defn- is-capitalized? [sym]
  (let [c (first (name sym))]
    #?(:clj  (Character/isUpperCase (char c))
       :cljs (= c (.toUpperCase c)))))

(def default-extenders
  (fn [db]
    [(fn [node]
       (assoc node :model.node/is-capitalized?
                   (is-capitalized? (:model.node/symbol node))))]))

(defn comp-extenders [ext1 ext2]
  (if (and ext1 ext2)
    (fn [db] (into (ext1 db) (ext2 db)))
    (or ext1
        ext2
        (constantly nil))))

(defn index-model
  ([meta-db model]
   (index-model meta-db model {}))
  ([meta-db model {:keys [extenders]}]
   (let [find-root (memoize-found-id (partial find-root meta-db))
         find-node (memoize-found-id (partial find-node meta-db))
         extenders! ((comp-extenders default-extenders extenders)
                      meta-db)

         add-parent
         (fn self [selectors parent]
           (->> selectors
                (map #(assoc % :parent parent
                               :db/id (find-node parent (:sym %))
                               :meta (meta (:sym %))))
                (map #(update % :selectors self (:db/id %)))))

         node->model-node
         (fn self [{:keys [sym selectors db/id meta parent]}]
           (let [tag (:tag meta)
                 node (cond-> {:db/id             id
                               :model.node/parent parent
                               :model.node/symbol sym}
                              (seq meta)
                              (assoc :model.node/meta meta))]
             (cons node (mapcat self selectors))))]

     (->> (s/conform ::model model)
          (map (fn [{:keys [sym] :as root}]
                 (let [db-id (find-root sym)]
                   (-> (assoc root :db/id db-id
                                   :meta (meta sym))
                       (update :selectors add-parent db-id)))))
          (mapcat
            (fn [{:keys [sym meta db/id selectors]}]
              (let [node (cond-> {:db/id             id
                                  :model.node/symbol sym}
                                 (seq meta)
                                 (assoc :model.node/meta meta))]
                (cons node (mapcat node->model-node selectors)))))
          (map (fn [node]
                 (reduce #(%2 %1) node extenders!)))
          (d/db-with meta-db)))))

(defn model->datascript-schema-2 []
  (let [meta-db (-> (init-meta-db)
                    (index-model model-schema-model))]
    (d/q '{:find [(pull ?e [{}])]})))

(defn- query-with-rules [query-map inputs rules]
  (let [in (:in query-map)
        has-rules? (some #{'%} in)
        inputs (if has-rules?
                 (update (vec inputs) 0 into rules)
                 (cons rules inputs))
        in (cond
             has-rules? in
             (seq in) (cons (first in) (cons '% (rest in)))
             :else '[$ %])]
    [inputs (assoc query-map :in in)]))

(def query-rules
  ['[(root-node ?node ?sym)
     [(missing? $ ?node :model.node/parent)]
     [?node :model.node/symbol ?sym]]
   ;; Bind k and v to attributes in node's meta
   '[(node-meta ?node ?meta-k ?meta-v)
     [?node :model.node/meta ?meta]
     [?meta ?meta-k ?meta-v]]
   ;; Extract type of a node. Either check its meta tag
   ;; or check whether it's the root node and it is capitalized.
   '[(node-type ?node ?type)
     (node-meta ?node :tag ?type)]
   '[(node-type ?node ?type)
     [?node :model.node/symbol ?sym]
     (root-node ?node ?sym)
     [?node :model.node/is-capitalized? ?cap]
     [(true? ?cap)]
     [(identity ?sym) ?type]]])

(defn q
  "Query a model db with some predefined rules."
  [query-map model-db & inputs]
  {:pre [(= (count inputs)
            (dec (count (:in query-map '[$]))))]}
  (let [[inputs query] (query-with-rules query-map inputs query-rules)]
    (apply d/q query model-db inputs)))
