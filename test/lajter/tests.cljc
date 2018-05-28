(ns lajter.tests
  (:require
    [clojure.test]
    [lajter.core-test]
    [lajter.git_test]
    [expound.alpha]
    [clojure.spec.alpha :as s]
    [clojure.spec.test.alpha :as st]))

(s/check-asserts true)
(set! s/*explain-out* expound.alpha/printer)

(defn -main [& args]
  (clojure.test/run-all-tests #"lajt.*"))

(st/instrument)
