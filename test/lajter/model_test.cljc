(ns lajter.model-test
  (:require
    [clojure.test :refer [deftest is are testing]]
    [lajter.model :as model]))

(deftest model-index-test
  (let [meta-db (model/init-meta-db)]
    (testing "Root level capitalized symbols are themselves its type."
      (is (= 'Person
            (model/q
              '{:find  [?type .]
                :in    [$ ?root]
                :where [(root-node ?e ?root)
                        (node-type ?e ?type)]}
              (model/index-model meta-db '[Person])
              'Person))))
    (testing "Root level fields do not have a default type"
      (is (nil?
            (model/q
              '{:find  [?type .]
                :in    [$ ?root]
                :where [(root-node ?e ?root)
                        (node-type ?e ?type)]}
              (model/index-model meta-db '[first-name])
              'first-name))))
    (def meta-db meta-db)
    (testing "Fields in entities can have types with metadata"
      (is (= 'String
             (model/q
               '{:find  [?type .]
                 :in    [$ ?sym]
                 :where [(root-node ?e ?sym)
                         [?field :model.node/parent ?e]
                         (node-type ?field ?type)]}
               (model/index-model meta-db '[Person
                                            [^String first-name]])
               'Person))))))
