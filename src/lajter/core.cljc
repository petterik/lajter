(ns lajter.core
  (:require
    #?@(:cljs [[react]
               [create-react-class]
               [react-dom]
               [goog.object :as gobj]])))

(def foo 1)

(def class
  {:render
   (fn [props]
     [:div "foo"])})

;; Make this trigger. We need a main.
