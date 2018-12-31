(ns lajter.model.graphql-schema-test
  (:require
    [clojure.test :refer :all]
    [clojure.java.io :as jio]
    [clojure.data.json :as json]
    [lajter.model :as model]
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
  (time
    (let [db (model/init-meta-db [graphql/plugin:graphql])
          model (-> (gql-schema)
                    (get-in [:data :__schema :types])
                    (graphql/schema->model))]
      (time
        (let [model-db (model/index-model db model)]
          #_(count (time (db/datascript-schema model-db)))
          (binding [pull/*full-pattern-max-depth* 3]
            (time
              #_(pull/full-pattern model-db 'Query)
              (-> (graphql/datafy model-db))
              )))))))


(comment
  (require '[clojure.core.protocols :as p])
  ((fn wrap-nav [m]
     (with-meta m {`p/nav (fn [m k _]
                            (wrap-nav (update m k inc)))}))
    {:a 1 :b 2})

  (p/nav *1 :a nil)
  ;; {:a 2 :b 2}

  )


#_(defn profile-indexing []
  (let [s (gql-schema)
        db (model/init-meta-db)
        model (graphql/schema->model s)]
    (model/index-model db model)))
