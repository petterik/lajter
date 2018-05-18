(ns lajter.protocols)

(defprotocol IReconciler
  (reconcile! [this])
  (schedule-render! [this])
  (schedule-sends! [this])
  (queue-sends! [this remote-target query])
  (send! [this]))