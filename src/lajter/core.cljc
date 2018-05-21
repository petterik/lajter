(ns lajter.core
  (:require
    [lajter.react]
    [lajter.logger :refer [log]]
    [lajter.protocols :as p]
    [lajter.history :as history]
    [lajt.parser]
    [lajt.read]
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
  (let [spec (cond-> x (not (map? x)) p/spec-map)]
    (not-empty
      (into (:lajter/query spec [])
            (mapcat get-query)
            (:lajter/children spec)))))

(defn get-root [reconciler]
  (get-in reconciler [:config :root-component]))

(defn get-root-query [reconciler]
  (get-query (get-root reconciler)))

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
  (log " getting reconciler from : " x)
  (let [reconciler (p/get-reconciler x)
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
  p/IHasReconciler
  (get-reconciler [this] this)
  IStoppable
  (stop! [this]
    (remove-watch (:app-state config) ::reconciler))
  IEnvironment
  (to-env [this]
    (select-keys config [:parser :state :remotes]))
  p/IReconciler
  (react-class [this component-spec]
    (if-let [klass (get (:class-cache @state) component-spec)]
      klass
      (let [klass (lajter.react/create-class component-spec)]
        (swap! state update :class-cache assoc component-spec klass)
        klass)))
  (reconcile! [this]
    (swap! state dissoc :scheduled-render?)
    (let [{:keys [parser root-render target root-component]} config
          root-class (p/react-class this root-component)
          props (parser (to-env this) (get-query root-class))]
      (root-render
        (lajter.react/create-instance this root-class props)
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
    (log "SEND!: " @state)
    (let [[old _] (swap-vals! state assoc
                              :queued-sends {}
                              :scheduled-sends? nil)]
      (doseq [[target query] (:queued-sends old)]
        (when (seq query)
          (let [cb (fn [value]
                     ((:merge-fn config) this value query target))]
            ((:send-fn config) this cb query target)))))))

(defn mount [{:as   config}]
  (let [parser (lajt.parser/parser
                 {:lajt.parser/query-plugins
                  [(lajt.parser/dedupe-query-plugin {})]
                  :read
                  (lajt.read/->read-fn
                    (fn [k]
                      {:custom (fn [env] (get @(:state env) k))
                       :remote true})
                    {})
                  :mutate
                  (fn [env k p]
                    (if (:target env)
                      [true (keyword (namespace k))]
                      (condp = k
                        'foo/conj
                        (swap! (:state env) update :foo conj (rand-int 100))
                        'foo/pop
                        (swap! (:state env) update :foo pop)
                        'bar/add
                        (swap! (:state env) update :bar assoc
                               (rand-int 100)
                               (rand-int 100)))))})
        remote-state (atom @(:state config))
        send-fn (fn [reconciler cb query target]
                  (log "would send query: " query
                       " to target: " target)
                  #?(:cljs
                     (js/setTimeout
                       #(let [remote-parse (parser (assoc (to-env reconciler)
                                                     :state remote-state)
                                                   query)]
                          (cb remote-parse))
                       2000)))
        merge-fn (fn [reconciler value query target]
                   (swap! (get-in reconciler [:config :state])
                          merge
                          value))
        r (->Reconciler (assoc config :parser parser
                                      :send-fn send-fn
                                      :merge-fn merge-fn)
                        (atom {}))]
    (log "state: " (:state config))
    (add-watch (:state config) ::reconciler
               (fn [k ref old-state new-state]
                 (schedule-render! r)))
    r))

(defn render-child [this child-component]
  (let [reconciler (p/get-reconciler this)]
    (lajter.react/create-instance
      reconciler
      (p/react-class reconciler child-component)
      (p/raw-clj-props this))))

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
