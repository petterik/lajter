(ns lajter.model.gen
  (:require
    [clojure.test.check.generators :as gen]
    [datascript.core :as d]
    [lajter.model :as model]))

(defn entity [model-db sym]
  (->> (model/q '{:find  [[(pull ?field [:model.node/symbol
                                         {:model.node/meta [:tag]}])
                           ...]]
                  :in    [$ ?sym]
                  :where [(entity-fields ?sym ?field)]}
                model-db
                sym)
       (map (fn [{:model.node/keys [symbol meta]}]
              [(keyword (name sym) (name symbol)) (:tag meta)]))
       (into {})))

;; throws ClassCastException "Symbol cannot be cast to String"

(defn entities [meta-db roots]
  )