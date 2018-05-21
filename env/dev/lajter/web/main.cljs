(ns ^:figwheel-hooks lajter.web.main
  (:require
    [devtools.core :as devtools]
    [lajter.logger :refer [log]]
    [lajter.core :as la]
    [lajter.react :as re]
    [lajter.protocols :as p]
    [react-dom :as react-dom]
    [om.dom :as dom]))


(def ComponentB
  {:query [:foo]
   :displayName "B"
   :render (fn [this props state]
             (dom/div nil
               (dom/p nil " Component B")
               (dom/p nil " Props: " props)))})

(def ComponentA
  {:query
   [:foo {:bar [:a]}]
   :children [ComponentB]
   :displayName "A"
   :render
   (fn [this props state]
     (dom/div
       nil
       (dom/p nil (str "props: " props))
       (dom/p nil (str " state: " state))
       (dom/p nil (str " counter: " (:counter state)))
       (dom/button #js {:onClick
                        #(la/update-state! this update :counter inc)})
       (la/render-child this ComponentB)))
   :getDerivedStateFromProps
   (fn [_ props state]
     (log "in derived state. Props: " props)
     {:initial-state (get-in props [:bar :a])
      :counter       0})})


(defn ^:after-load runit! []
  (let [config {:root-component (lajter.react/create-class ComponentA)
                :root-render react-dom/render
                :target      (.getElementById js/document "app")}]

    (reset! la/reconciler-atom nil)
    (lajter.core/reloaded config)))

(defn -main [& args]
  (enable-console-print!)
  (devtools/install!)
  (runit!))

(defonce call-main-once (-main))
(log *ns* " loaded")
