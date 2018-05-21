(ns lajter.protocols)

(defprotocol IReconciler
  (reconcile! [this])
  (react-class [this component-spec])
  (schedule-render! [this])
  (schedule-sends! [this])
  (queue-sends! [this remote-target query])
  (send! [this]))

(defprotocol IHasReconciler
  (get-reconciler [this]))

(defprotocol IReactElement
  (raw-clj-props [this])
  (clj-props [this] "returns the clojure props for this element.")
  (clj-state [this])
  (update-clj-state! [this f]))

(defprotocol ILajterClass
  (spec-map [this]))
