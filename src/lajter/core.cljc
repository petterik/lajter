(ns lajter.core
  (:require
    [lajter.react]
    [lajter.layer :as layer]
    [lajter.logger :refer [log]]
    [lajter.protocols :as p]
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

(defn get-computed
  ([this]
   (p/clj-computed this))
  ([this k]
    (get (p/clj-computed this) k)))

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

(defn query-keys [query]
  (into #{}
        (map :lajt.parser/key)
        (lajt.parser/query->parsed-query query)))

;; Make this trigger. We need a main.

;; TODO: Replace with component's protocol.
(defprotocol IStoppable
  (stop! [this]))

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

(defn- gen-tx-id! []
  #?(:cljs (random-uuid)
     :clj  (java.util.UUID/randomUUID)))

(defn transact! [x query]
  (let [reconciler (p/get-reconciler x)
        tx-id (gen-tx-id!)
        layer (-> (layer/transaction-layer reconciler query)
                  (layer/with-id tx-id))]
    (p/add-layer! reconciler layer)
    (if (seq (:layer.remote/targets layer))
      (schedule-sends! reconciler)
      (p/squash-local-layers! reconciler))
    (schedule-render! reconciler)))

(defrecord Reconciler [config state]
  p/IHasReconciler
  (get-reconciler [this] this)
  IStoppable
  (stop! [this]
    (remove-watch (:app-state config) ::reconciler))
  p/IEnvironment
  (to-env [this]
    (select-keys config [:parser :state :remotes]))
  p/ILayers
  (add-layer! [this layer]
    (swap! state update :layers layer/add-layer layer))
  (replace-layer! [this layer-id with-layer]
    (swap! state update :layers layer/replace-layer layer-id with-layer))
  (squash-local-layers! [this]
    (let [layers (:layers @state)]
      (when-let [local-layers (seq (layer/leading-local-layers layers))]
        (swap! (:state config) #(layer/db-with-layers this % local-layers))
        (swap! state update :layers layer/drop-layers local-layers))))

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
          db (deref (:state config))
          layers (:layers @state [])
          _ (log "layers: " {:layers layers})
          db (layer/db-with-layers this db layers)
          env (assoc (p/to-env this) :state (atom db) :db db)
          props (parser env (get-query root-class))]
      (root-render
        (lajter.react/create-instance this root-class props nil)
        target)))
  (schedule-render! [this]
    (let [[old _] (swap-vals! state assoc :scheduled-render? true)]
      (not (:scheduled-render? old))))
  (schedule-sends! [this]
    (let [[old _] (swap-vals! state assoc :scheduled-sends? true)]
      (not (:scheduled-sends? old))))
  (send! [this]
    (log "SEND!: " @state)
    (let [remote-layer (layer/first-remote-unsent (:layers @state))]
      (swap! state #(-> (assoc % :scheduled-sends? nil)
                        (cond-> (some? remote-layer)
                                (update :layers layer/mark-sent-layer remote-layer))))
      (log "Remote-layer: " remote-layer)
      (doseq [target (:layer.remote/targets remote-layer)]
        (let [query (into (get-in remote-layer [:layer.remote/mutates target] [])
                          (get-in remote-layer [:layer.remote/reads target]))]
          (let [cb (fn [value]
                     (p/replace-layer! this
                                     (:layer/id remote-layer)
                                     (layer/->merge-layer remote-layer value))
                     (p/squash-local-layers! this)
                     (schedule-render! this)
                     (schedule-sends! this))]
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
                      (when (= "foo" (namespace k))
                        [true :foo])
                      (condp = k
                        'foo/conj
                        (swap! (:state env) update :foo conj (:x p))
                        'foo/pop
                        (swap! (:state env) update :foo pop)
                        'bar/assoc
                        (swap! (:state env) update :bar assoc (:k p) (:v p))
                        'bar/dissoc
                        (swap! (:state env) update :bar #(dissoc % (first (keys %)))))))})
        remote-state (atom @(:state config))
        send-fn (fn [reconciler cb query target]
                  (log "would send query: " query
                       " to target: " target)
                  #?(:cljs
                     (js/setTimeout
                       #(let [remote-parse
                              (parser (assoc (p/to-env reconciler) :state remote-state)
                                      (->> query
                                           (lajt.parser/query->parsed-query)
                                           (map (fn [{:lajt.parser/keys [key] :as m}]
                                                  (if (= 'foo/conj key)
                                                    (update-in m [:lajt.parser/params :x] inc)
                                                    m)))
                                           (lajt.parser/parsed-query->query)))]
                          (cb (into {}
                                    (remove (comp symbol? key))
                                    remote-parse)))
                       2000)))
        merge-fn (fn [reconciler db value tx-info]
                   (merge db value))

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
  (let [reconciler (p/get-reconciler this)
        computed (some-> (p/spec-map this)
                         :lajter/computed
                         (get child-component)
                         (as-> f (f this)))]
    (lajter.react/create-instance
      reconciler
      (p/react-class reconciler child-component)
      (p/raw-clj-props this)
      computed)))

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
