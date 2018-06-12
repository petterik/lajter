(ns lajter.protocols)

(defprotocol IBasis
  (basis-t [this]))

(defprotocol IReconciler
  (reconcile! [this])
  (react-class [this component-spec])
  (schedule-render! [this])
  (schedule-sends! [this])
  (get-config [this] [this k])
  (send! [this])
  (select-route [this route-data])
  (query-params [this]))

(defprotocol IIndexer
  (index-component! [this component])
  (drop-component! [this component])
  (is-indexed? [this component])
  (components-to-render [this all-props all-routing prev-props prev-routing]))

(defprotocol IEnvironment
  (to-env [this]))

(defprotocol IHasReconciler
  (get-reconciler [this]))

(defprotocol IReactClass
  (create-element [this props]))

(defprotocol IReactElement
  (all-clj-props [this])
  (all-clj-routes [this])
  (clj-props [this] "returns the clojure props for this element.")
  (clj-state [this])
  (update-clj-state! [this f])
  (clj-computed [this])
  (clj-routes [this])
  (depth [this])
  (force-update! [this]))

(defprotocol ILajterClass
  (spec-map [this]))

(defprotocol ILayers
  (add-layer! [this layer])
  (replace-layer! [this tx-id with-layer]))