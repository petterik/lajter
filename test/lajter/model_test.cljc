(ns lajter.model-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [lajter.model :as model]))

(def model-schema
  {:model.type/symbol {:db/unique :db.unique/identity}
   :model.node/symbol {:db/index true}
   :model.node/parent {:db/valueType :db.type/ref}
   :model.node/type   {:db/valueType :db.type/ref}
   :model.node/meta   {:db/valueType :db.type/ref}})

(deftest can-define-datascript-model-in-its-own-syntax
  (is (= model-schema
         (model/model->datascript-schema model/model-schema-model))))

(deftest model-index-test
  (let [meta-db (model/init-meta-db)]
    (testing "Capitalized root nodes have their own types"
      (= 'Person
         (model/q
           '{:find  [?type .]
             :in [$ ?root]
             :where [(root-node ?e ?root)
                     (node-type ?e ?type)]}
           (model/index-model meta-db '[Person])
           'Person)))))