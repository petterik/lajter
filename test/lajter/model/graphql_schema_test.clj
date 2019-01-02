(ns lajter.model.graphql-schema-test
  (:require
    [clojure.test :refer :all]
    [clojure.java.io :as jio]
    [clojure.data.json :as json]
    [datascript.core :as d]
    [lajter.model :as model]
    [lajter.model.datafy :as datafy]
    [lajter.model.gen :as gen]
    [lajter.model.graphql :as graphql]
    [lajter.model.db :as db]
    [lajter.model.pull :as pull]
    [clojure.core.protocols :as p]))

(def gql-schema-file
  ^{:doc    "This schema was acquired by introspecting github's
  graphql API by querying it. The shape of the schema
  is found in github-schema.gql"
    :source "https://developer.github.com/v4/explorer/"
    :query  "test/graphql/github-schema.gql"}
  (jio/resource "test/graphql/github-schema.json"))

(defn gql-schema []
  (graphql/parse-schema-str (slurp gql-schema-file)))

(defn gql-schema->model-db []
  (time
    (let [db (model/init-meta-db [graphql/plugin:graphql])
          model (-> (gql-schema)
                    (graphql/schema->model))]
      (model/index-model db model))))

(comment
  ;; For REBL use:
  (datafy/datafy (gql-schema->model-db))

  (def model-db *1)

  (count )
  (require '[clojure.edn :as edn])
  (edn/read-string {:readers (merge *data-readers* d/data-readers)} (pr-str model-db))




  (db/datascript-schema model-db)

  (def schema *1)
  (count schema)

  )
