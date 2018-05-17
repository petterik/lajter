(ns lajter.core
  (:require
    [lajter.logger :refer [log]]
    [om.dom :as dom]
    #?@(:cljs [[react :as react]
               [react-dom :as react-dom]
               [create-react-class :as create-react-class]
               [goog.object :as gobj]])
    ))

(def foo 1)

(def react-class
  {:render
   (fn [props]
     [:div "foo"])})

;; Make this trigger. We need a main.

(defn experiment []
  #?(:cljs
     (let [klass (create-react-class
                   #js {:displayName           "experiment"
                        :render                (fn []
                                                 (dom/div nil "Rendering experiment :D"))
                        :shouldComponentUpdate (fn [] true)
                        })
           elem (react/createElement klass #js {})]
       (log klass)
       (log elem)
       (log (.getElementById js/document "app"))
       (let [app-div (.getElementById js/document "app")]
         (react-dom/render elem app-div)))))

(defn reloaded []
  (log "RELOADED :D")

  (experiment))
