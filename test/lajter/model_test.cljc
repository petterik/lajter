(ns lajter.model-test
  (:require
    [clojure.test :refer [deftest is]]
    [lajter.model :as model]))

(def model-schema
  {:model.entity/type {:db/valueType :db.type/ref}
   :model.entity/symbol {:db/index true}
   :model.type/symbol {:db/unique :db.unique/identity}
   :model.field/symbol {:db/index true}
   :model.field/parent {:db/valueType :db.type/ref}
   :model.field/type {:db/valueType :db.type/ref}})

(deftest can-define-datascript-model-in-its-own-syntax
  (is (= model-schema
         (model/model->datascript-schema model/model-schema-model))))
