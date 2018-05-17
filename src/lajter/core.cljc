(ns lajter.core
  (:require
    [clojure.set :as set]
    #?(:cljs [react :as react])
    ))

(def foo 1)

#_(def class
  {:render
   (fn [props]
     [:div "foo"])})

;; Make this trigger. We need a main.

(defn reloaded []
  (prn "RELOADED :D")
  (prn "LOL")
  (prn "FOOOOOOOOLxKDSOK")
  #?(:cljs (prn react/render)))
