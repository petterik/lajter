(ns lajter.model.gen
  (:require
    [clojure.spec.alpha :as s]
    [clojure.test.check.generators :as gen]
    [datascript.core :as d]
    [lajter.model :as model]
    [lajter.model.db :as db]))

;; TODO: Make this pluggable.
;; The values are functions taking the field they came from.
;; We can use the metadata on the field to better generate fields.
(def known-generators
  {'String     (constantly gen/string)
   'Keyword    (constantly gen/keyword-ns)
   'UUID       (constantly gen/uuid)
   'ID         (constantly (gen/fmap #(str "ID-" %) gen/uuid))
   'Integer    (constantly gen/int)
   'Float      (constantly (gen/fmap float gen/double))
   'BigDecimal (constantly (gen/fmap (comp bigdec float)
                                     (gen/double* {:infinite? false
                                                   :NaN? false
                                                   :min -1e5
                                                   :max 1e5})))})

(defn- field-types
  "Given an type symbol, finds all the fields defined for the type
  and returns them in a map of {type-sym/field type-sym}."
  [model-db type-sym]
  (->> (model/q '{:find  [[?field ...]]
                  :in    [$ ?sym]
                  :where [(type-fields ?sym ?field)]}
                model-db
                type-sym)
       (map (fn [field]
              (let [e (d/entity model-db field)
                    m (into {} (:model.node/meta e))]
                [(with-meta (symbol (name type-sym)
                                    (name (:model.node/symbol e)))
                            m)
                 (:tag m)])))
       (into {})))

(defn- type-map
  "Constructs a map with {entyt-sym {field type-sym}}, starting
  from the specified root type symbol."
  [model-db root-sym]
  (let [m (field-types model-db root-sym)]
    (->> (sorted-map root-sym m)
         (iterate
           (fn [entities]
             (into entities
                   (comp
                     cat
                     (map val)
                     (remove #(contains? entities %))
                     (map (fn [type-sym]
                            [type-sym (field-types model-db type-sym)])))
                   (vals entities))))
         (partition 2 1)
         (drop-while (fn [[a b]] (not= (count a) (count b))))
         ffirst)))

(defn entity
  "Takes a model-db and a symbol of an entity. Returns a generator
  that generates the entity based on the model, including all the
  leaves."
  [model-db root-sym]
  (let [types (type-map model-db root-sym)

        known-gen
        (fn [field sym]
          (or ((get known-generators sym) field)
              (throw (ex-info
                       (str "Symbol does not have a known generator: " sym)
                       {:sym sym}))))

        many-field?
        (fn [field]
          (let [m (meta field)]
            (or (true? (:db.cardinality/many m))
                (= (:db/cardinality m) :db.cardinality/many))))

        field-gen
        (fn self [[field sym]]
          (let [v (if-let [fields (not-empty (get types sym))]
                    (apply gen/hash-map (mapcat self fields))
                    (known-gen field sym))
                v (cond-> v (many-field? field) (gen/vector 1 3))]
            [(keyword field) v]))]
    (second
      (field-gen [nil root-sym]))))

(defn entities
  "Takes a coll of root symbols to generate returns a generator
  for generating tuples of the roots."
  [meta-db pattern]
  ;; TODO: Select only what the pattern wants.
  ;; Generates the entire pattern for now.
  ;; Idea: Filter meta-db such that it only contains the fields
  ;; from the pattern?
  ;; OR: filter the fields to be generated by the pattern.
  ;; Possibly, separate the gen step from the creation of the tree
  ;; step.
  ;; Idea: Create an ast of what's about to be generated then postwalk
  ;; the tree and do the selection?
  ;; ^ This.
  (->> (s/conform ::model/model pattern)
       (map :sym)
       (map (partial entity meta-db))
       (apply gen/tuple)))

(def sample gen/sample)
