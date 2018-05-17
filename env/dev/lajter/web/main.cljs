(ns ^:figwheel-hooks lajter.web.main
  (:require
    [lajter.core]))

(defn -main [& args]
  (lajter.core/reloaded))

(defn ^:after-load on-reload! []
  (-main))

(-main)
(prn *ns* " loaded")