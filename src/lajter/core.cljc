(ns lajter.core
  (:require
    [lajter.react]
    [lajter.logger :refer [log]]
    [lajter.protocols :as p]
    [lajter.history :as history]
    [lajt.parser]
    [clojure.set :as set]
    [om.dom :as dom]
    #?@(:cljs [[goog.object :as gobj]])))

(defn get-state [this]
  (p/clj-state this))

(defn get-props [this]
  (p/clj-props this))

(defn update-state! [this f & args]
  (p/update-clj-state! this #(apply f % args)))

(defn set-state! [this value]
  (update-state! this (fn [_] value)))

(defn reconciler? [x]
  (satisfies? p/IReconciler x))

(defn component? [x]
  (and (instance? p/IReactElement x)
       (some? (p/clj-props x))))

(defn get-query [x]
  (p/query x))

(defn get-full-query [reconciler]
  (get-query reconciler))

;; Make this trigger. We need a main.

;; TODO: Replace with component's protocol.
(defprotocol IStoppable
  (stop! [this]))

(defprotocol IEnvironment
  (to-env [this]))

(def ^:dynamic *raf*)

#?(:cljs
   (defn- queue-render! [f]
     (cond
       (fn? *raf*) (*raf* f)
       (not (exists? js/requestAnimationFrame))
       (js/setTimeout f 16)
       :else
       (js/requestAnimationFrame f))))

(defn schedule-render! [reconciler]
  #?(:cljs
     (when (p/schedule-render! reconciler)
       (queue-render! #(p/reconcile! reconciler)))))

(defn schedule-sends! [reconciler]
  (when (p/schedule-sends! reconciler)
    #?(:cljs
       (p/send! reconciler))))

(defn transact! [x query]
  (let [reconciler (cond-> x (component? x) (p/get-reconciler))
        {:keys [history parser remotes state] :as env} (to-env reconciler)
        #?@(:cljs [history-id (random-uuid)] :clj
                  [history-id (java.util.UUID/randomUUID)])
        _ (when (some? history)
            (history/add! history history-id (deref state)))
        ;; Perform mutations
        local-parse (parser (assoc env ::history-id history-id)
                            query
                            nil)
        remote-parses (into {}
                            (map (juxt identity #(parser env query %)))
                            remotes)]
    (doseq [[target remote-query] remote-parses]
      (p/queue-sends! reconciler target remote-query))
    (schedule-sends! reconciler)))

(defrecord Reconciler [config state]
  IStoppable
  (stop! [this]
    (remove-watch (:app-state config) ::reconciler))
  IEnvironment
  (to-env [this]
    (select-keys config [:parser :state]))
  p/IQuery
  (query [_] (p/query (:root-component config)))
  p/IReconciler
  (reconcile! [this]
    (swap! state dissoc :scheduled-render?)
    (let [{:keys [parser root-render target root-component]} config
          props (parser (to-env this) (get-query root-component))]
      (root-render
        (lajter.react/create-instance this root-component props)
        target)))
  (schedule-render! [this]
    (let [[old _] (swap-vals! state assoc :scheduled-render? true)]
      (not (:scheduled-render? old))))
  (schedule-sends! [this]
    (let [[old _] (swap-vals! state assoc :scheduled-sends? true)]
      (not (:scheduled-sends? old))))
  (queue-sends! [this remote-target query]
    (swap! state update-in [:queued-sends remote-target]
           (fnil into []) query))
  (send! [this]
    (let [[old _] (swap-vals! state assoc
                              :queued-sends {}
                              :scheduled-sends? nil)]
      (doseq [[target query] (:queued-sends old)]
        ((:send-fn config) this target query)))))

(defn mount [config]
  (let [app-state (atom {:foo [1 2 3 4]
                         :bar {:a :b}})
        send-fn (fn [target query]
                  (log "would send query: " query
                       " to target: " target))
        parser (lajt.parser/parser
                 {:read   (fn [env k _]
                            (when (nil? (::history-id env))
                              (get @(:state env) k)))
                  :mutate (fn [env k p]
                            (when (nil? (:target env))
                              (condp = k
                                'foo/conj
                                (swap! (:state env) update :foo conj (rand-int 100))
                                'bar/add
                                (swap! (:state env) update :bar assoc
                                       (rand-int 100)
                                       (rand-int 100)))))})
        r (->Reconciler (assoc config :state app-state
                                      :parser parser
                                      :send-fn send-fn)
                        (atom {}))]
    (add-watch app-state ::reconciler
               (fn [k ref old-state new-state]
                 (schedule-render! r)))
    r))

(defn get-root [reconciler]
  (get-in reconciler [:config :root-component]))

(defonce reconciler-atom (atom nil))
(defn redef-reconciler [config]
  (when-let [r @reconciler-atom]
    (stop! r))
  (reset! reconciler-atom (mount config)))

(defn reloaded [config]
  (log "RELOADED :D")
  (when (or (nil? @reconciler-atom))
    (redef-reconciler config))

  (schedule-render! @reconciler-atom))
