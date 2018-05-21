(ns ^:figwheel-hooks lajter.web.main
  (:require
    [devtools.core :as devtools]
    [lajter.logger :refer [log]]
    [lajter.core]
    [react-dom :as react-dom]))

(def config {:root-render react-dom/render
             :target      (.getElementById js/document "app")})

(defn ^:after-load runit! []
  (lajter.core/reloaded config))

(defn -main [& args]
  (enable-console-print!)
  (devtools/install!)
  (runit!))

(defonce call-main-once (-main))
(log *ns* " loaded")
