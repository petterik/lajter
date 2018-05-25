(ns lajter.git_test
  (:require
    [lajter.git :as g]
    [clojure.test :as t :refer [deftest is are]]))

(deftest initial-value
  (let [g (g/git-client)]
    (is (empty? (g/log g)))))

(deftest index-and-commit-operations
  (let [g (g/git-client)]
    (is (nil? (g/read g :foo)))
    (is (= 1 (-> (g/add g :foo 1) (g/read :foo))))
    (is (= 1 (-> (g/add g :foo 1) (g/commit) (g/read :foo))))
    (is (= 1 (-> (g/change g :foo (fn [_] (-> 1))) (g/read :foo))))
    ))

(defn verify
  ([x y] (is (= x y)))
  ([x y msg] (is (= x y) msg)))

(deftest branch-test
  (let [g (g/git-client)]
    (is (= g (g/delete-branch g "master")))
    (is (= 1 (-> g (g/branch "foo") (g/add :foo 1) (g/read :foo))))
    (is (= 1 (-> g (g/branch "foo") (g/add :foo 1) (g/checkout "master") (g/read :foo))))
    (is (nil? (-> g (g/branch "foo") (g/add :foo 1) (g/commit)
                 (g/checkout "master") (g/read :foo))))
    (-> g
        (g/branch "BRANCH")
        (g/add :foo 1)
        (doto (-> (g/read :foo) (verify 1 "read index from in new branch")))
        (g/commit)
        (doto (-> (g/read :foo) (verify 1)))
        (doto (-> (g/checkout "master") (g/read :foo) (verify nil ":foo is only on BRANCH")))
        (doto (-> (g/checkout "master") (g/checkout "BRANCH") (g/read :foo)
                  (verify 1 "checking back and fourth between branches, can still read :foo")))
        (g/checkout "master")
        (g/add :foo 2)
        (doto (-> (g/checkout "BRANCH") (g/read :foo)
                  (verify 2 ":index is kept between branches")))
        (g/commit)
        (doto (-> (g/read :foo) (verify 2)))
        (doto (-> (g/checkout "BRANCH") (g/read :foo) (verify 1))))))

(deftest delete-branch-test
  (let [g (-> (g/git-client)
              (g/branch :branch)
              (g/add :foo 1)
              (g/commit)
              (g/checkout "master")
              (g/delete-branch :branch))
        commit-id 0]
    (is (nil? (g/read g :foo)))
    (is (nil? (-> g (g/branch :branch) (g/read :foo))))
    ;; Reading in headless state
    (is (= 1 (-> g (g/checkout commit-id) (g/read :foo))))
    (is (= 1 (-> g (g/cherry-pick commit-id) (g/read :foo))))
    (is (= (-> g (g/checkout commit-id) (g/branch :branch))
           (-> (g/git-client) (g/branch :branch) (g/add :foo 1) (g/commit))))))

(deftest gc-test
  )