(ns lajter.protocols)

(defprotocol IReconciler
  (reconcile! [this])
  (schedule-render! [this])
  (schedule-sends! [this])
  (queue-sends! [this remote-target query])
  (send! [this]))

(defprotocol IHasReconciler
  (get-reconciler [this]))

(defprotocol IReactElement
  (clj-props [this])
  (clj-state [this]))

(defprotocol IQuery
  (query [this]))
