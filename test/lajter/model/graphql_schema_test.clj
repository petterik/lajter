(ns lajter.model.graphql-schema-test
  (:require
    [clojure.test :refer :all]
    [clojure.java.io :as jio]
    [clojure.data.json :as json]
    [lajter.model :as model]
    [lajter.model.graphql :as graphql]))

(def gql-schema-file
  ^{:doc    "This schema was acquired by introspecting github's
  graphql API by querying it. The shape of the schema
  is found in github-schema.gql"
    :source "https://developer.github.com/v4/explorer/"
    :query  "test/graphql/github-schema.gql"}
  (jio/resource "test/graphql/github-schema.json"))

(defn gql-schema []
  (with-open [reader (jio/reader gql-schema-file)]
    (json/read reader
               :key-fn keyword
               :value-fn (fn omit [k v]
                           (cond
                             (nil? v) omit
                             (and (coll? v) (empty? v)) omit
                             (contains? #{:name :kind} k) (symbol v)
                             :else v)))))

(defn gql-schema->model-db []
  (-> (gql-schema)
      (get-in [:data :__schema :types])
      (graphql/schema->model)
      (->> (model/index-model (model/init-meta-db)))))

#_(defn profile-indexing []
  (let [s (gql-schema)
        db (model/init-meta-db)
        model (graphql/schema->model s)]
    (model/index-model db model)))
