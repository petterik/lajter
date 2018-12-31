(ns lajter.tests
  (:require
    [clojure.test]
    [lajter.core-test]
    [lajter.layer-test]
    [lajter.model-test]
    [expound.alpha :as expound]
    [clojure.spec.alpha :as s]
    [clojure.spec.test.alpha :as st]))

(s/check-asserts true)
(set! s/*explain-out* expound/printer)

(defn -main [& args]
  (clojure.test/run-all-tests #"lajter.*"))

(st/instrument)
