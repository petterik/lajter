(ns ^:figwheel-hooks lajter.web.main
  (:require
    [devtools.core :as devtools]
    [lajter.logger :refer [log]]
    [lajter.core :as la]
    [lajter.react :as re]
    [lajter.protocols :as p]
    [react-dom :as react-dom]
    [om.dom :as dom]))

(def experiment-component-map
  {:query
   [:foo {:bar [:a]}]
   :displayName
   "experiment"
   :render
   (fn [this props state]
     (dom/div
       nil
       (dom/span nil (str "props: " props))
       (dom/span nil (str " state: " state))
       (dom/span nil (str " counter: " (:counter state)))
       (dom/button #js {:onClick #(la/update-state! this update
                                                    :counter
                                                    inc)})))
   :getDerivedStateFromProps
   (fn [_ props state]
     (log "in derived state. Props: " props)
     {:initial-state (get-in props [:bar :a])
      :counter       0})})


(defn ^:after-load runit! []
  (let [ExperimentComponent (lajter.react/create-class
                              experiment-component-map)
        config {:root-component ExperimentComponent
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
