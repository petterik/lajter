(ns lajter.model-test
  (:require
    [clojure.test :refer [deftest is]]
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
