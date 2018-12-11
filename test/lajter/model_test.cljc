(ns lajter.model-test
  (:require
    [clojure.test :refer [deftest is are testing]]
    [lajter.model :as model]))

(defn index-model
  ([db model]
    (index-model db model nil))
  ([db model pipeline-opts]
   (let [pipeline
         (fn [_]
           [(map (fn [{:model.node/keys [symbol] :as node}]
                   (assoc node :model.node/keyword (keyword symbol))))])]
     (model/index-model db model {:pipeline pipeline
                                  :pipeline-opts pipeline-opts}))))

(def meta-db (model/init-meta-db {:model.node/keyword {:db/index true}}))

(deftest model-index-test
  (testing "Root level capitalized symbols are themselves its type."
    (is (= 'Person
           (model/q
             '{:find  [?type .]
               :where [[?e :model.node/keyword :Person]
                       (root-node ?e ?root)
                       (node-type ?e ?type)]}
             (index-model meta-db '[Person])))))
  (testing "Root level fields do not have a default type"
    (is (nil?
          (model/q
            '{:find  [?type .]
              :where [[?e :model.node/keyword :first-name]
                      (root-node ?e ?root)
                      (node-type ?e ?type)]}
            (index-model meta-db '[first-name])))))
  (testing "Fields in entities can have types with metadata"
    (is (= 'String
           (model/q
             '{:find  [?type .]
               :where [[?e :model.node/keyword :Person]
                       (root-node ?e _)
                       [?field :model.node/parent ?e]
                       (node-type ?field ?type)]}
             (index-model meta-db '[Person
                                    [^String first-name]])))))
  (testing "Merging tree"
    (let [model '[Person [^:foo name]
                  Person [^:bar name]]
          model-db (index-model meta-db model)]

      (testing "Only one node for path [Person name]"
        (is (= 1 (model/q '{:find  [(count ?e) .]
                            :where [[?p :model.node/keyword :Person]
                                    [?e :model.node/parent ?p]
                                    [?e :model.node/keyword :name]]}
                          model-db))))
      (testing "Merging metadata"
        (is (= {:foo true
                :bar true}
               (model/q
                 '{:find  [(pull ?meta [:foo :bar]) .]
                   :where [[?e :model.node/keyword :name]
                           [?e :model.node/meta ?meta]]}
                 model-db))))

      (testing "Merge custom collection"
        (let [model '[Person
                      ^{:sources [:remote :local]
                        :replace [1 2]} Person
                      ^{:sources [:github]
                        :replace [3 4]} Person]]
          (is (= {:sources [:remote :local :github]
                  :replace [3 4]}
                 (model/q
                   '{:find  [(pull ?meta [:sources :replace]) .]
                     :where [[?e :model.node/keyword :Person]
                             [?e :model.node/meta ?meta]]}
                   (index-model meta-db
                                model
                                {:merge-meta-fn
                                 (fn [{:keys [k]} a b]
                                   (if (= k :sources)
                                     (into a b)
                                     (or b a)))})))))))))
