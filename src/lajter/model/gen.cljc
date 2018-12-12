(ns lajter.model.gen
  (:require
    [clojure.test.check.generators :as gen]
    [datascript.core :as d]
    [lajter.model :as model]))

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
  (->> (model/q '{:find  [(pull ?field [:model.node/symbol
                                        {:model.node/meta [:tag]}])]
                  :in    [$ ?sym]
                  :where [(entity-fields ?sym ?field)]}
                model-db
                type-sym)
       (map (fn [[field]]
              (let [m (:model.node/meta field)]
                [(with-meta (symbol (name type-sym)
                                    (name (:model.node/symbol field)))
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

        ;; TODO: Handle the ^:many metadata, for generating a collection
        ;; of things.
        known-gen
        (fn [field sym]
          (or ((get known-generators sym) field)
              (throw (ex-info
                       (str "Symbol does not have a known generator: " sym)
                       {:sym sym}))))
        field-gen
        (fn self [[field sym]]
          (if-let [fields (not-empty (get types sym))]
            [field (apply gen/hash-map (mapcat self fields))]
            [field (known-gen field sym)]))]
    (second
      (field-gen [nil root-sym]))))

(defn entities
  "Takes a coll of root symbols to generate returns a generator
  for generating tuples of the roots."
  [meta-db roots]
  (apply gen/tuple (map (partial entity meta-db) roots)))

(def sample gen/sample)