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

;;;;;;;;;;;;;;;;;;;;;;;;
;; Component functions

(defn get-state [this]
  (p/clj-state this))

(defn get-props [this]
  (p/clj-props this))

(defn update-state! [this f & args]
  (p/update-clj-state! this #(apply f % args))
  (p/force-update! this))

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

(defn- get-child-computed [this child-component]
  (some-> (p/spec-map this)
          :lajter/computed
          (as-> f (f this))
          (get child-component)))

(defn render-child [this child-component]
  (let [reconciler (p/get-reconciler this)]
    (-> (lajter.react/create-element
          (p/react-class reconciler child-component)
          reconciler
          (inc (p/depth this))
          (p/all-clj-props this)
          (p/all-clj-routes this)
          (get-child-computed this child-component))
        #?(:clj lajter.react/call-render))))

(defn render-route [this route]
  (let [selected-route (get (p/clj-routes this) route)
        child-component (-> (p/spec-map this)
                            (:lajter/routing)
                            (get-in [route selected-route]))]
    (if (some? child-component)
      (render-child this child-component)
      (log "WARN: No child component for route: " route
           " selected-route: " selected-route
           " routing: " (:lajter/routing (p/spec-map this))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reconciler, query, routes, stuff

(defn route->component [reconciler route-data]
  (get (:lajter.routing/choices route-data)
       (p/select-route reconciler route-data)))

(defn routing-choices [spec]
  (eduction
    (map (fn [[route choices]]
           {:lajter.routing/route   route
            :lajter.routing/choices choices}))
    (:lajter/routing spec)))

(defn component-routes [reconciler x]
  (some->> (cond-> x (not (map? x)) (p/spec-map))
           (routing-choices)
           (not-empty)
           (into {}
                 (map (juxt :lajter.routing/route
                            #(p/select-route reconciler %))))))

(defn get-query [x]
  (:lajter/query (cond-> x (not (map? x)) p/spec-map)))

(defn get-full-query [reconciler x]
  (let [spec (cond-> x (not (map? x)) (p/spec-map))]
    (not-empty
      (into (:lajter/query spec [])
            (mapcat #(get-full-query reconciler %))
            (eduction
              cat
              [(:lajter/children spec)
               (eduction
                 (keep #(route->component reconciler %))
                 (routing-choices spec))])))))

(defn get-full-routing [reconciler x]
  (let [spec (cond-> x (not (map? x)) (p/spec-map))
        routing (component-routes reconciler x)]
    (into (or routing {})
          (map #(get-full-routing reconciler %))
          (eduction
            cat
            [(:lajter/children spec)
             (eduction
               (keep (fn [[route selected-route]]
                      (get-in spec [:lajter/routing route selected-route])))
               routing)]))))

(defn get-root [reconciler]
  (p/get-config reconciler :root-component))

(defn get-root-query [reconciler]
  (get-full-query reconciler (get-root reconciler)))

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
    (p/send! reconciler)))

(defn- gen-tx-id! []
  #?(:cljs (random-uuid)
     :clj  (java.util.UUID/randomUUID)))

(defn transact! [x query]
  (let [reconciler (p/get-reconciler x)
        tx-id (gen-tx-id!)
        layer (-> (layer/transaction-layer reconciler query)
                  (layer/with-id tx-id)
                  (layer/with-query-params (p/query-params reconciler)))]
    (log "ADDING LAYER: " layer)
    (p/add-layer! reconciler layer)
    (if (seq (:layer.remote/targets layer))
      (schedule-sends! reconciler))
    (schedule-render! reconciler)))

(defn latest-db-from-layers! [reconciler env]
  (let [#?@(:clj  [id (java.util.UUID/randomUUID)]
            :cljs [id (random-uuid)])
        _ (log "will get top-layers: " id)
        state (:state reconciler)
        current-state @state
        {:keys [snapshot layers] :as top-layers}
        (layer/top-layers (:layers current-state))
        db (cond-> (:layer.snapshot/db snapshot)
                   (seq layers)
                   (layer/db-with-layers reconciler env layers))]
    (log "got top-layers: " id " " top-layers)
    ;; when there are layers on top of the snapshot, make
    ;; a new snapshot and (if prod) remove the previous snapshots.
    (if (seq layers)
      (let [new-snapshot (layer/->snapshot-layer db)
            new-state (update current-state :layers layer/add-layer new-snapshot)
            success? (compare-and-set! state current-state new-state)]
        ;; GROSS GROSS GROSS
        ;; TODO: this has to be cleaned up.
        ;; Think about it. Snapshots don't actually add anything to the layers
        ;; It's just a caching mechanism.
        ;; Should it go onto the same datastructure as the actual layers?
        ;; Should it be calculated when env is trying to be gotten?
        ;; Don't think so.
        ;; Think about this.
        ;; (make Layers it's own thing to isolate the whole thing?)
        ;; Separate the functions and effects. Come on now.
        (if success?
          db
          (recur reconciler env)))
      db)))

(defrecord Reconciler [config state]
  p/IHasReconciler
  (get-reconciler [this] this)
  IStoppable
  (stop! [this]
    (remove-watch (:app-state config) ::reconciler))
  p/IEnvironment
  (to-env [this]
    ;; TODO: Fix recursion here.
    ;; latest-db -> db-with-layers -> to-env -> latest-db
    (let [app-state (:state config)
          env (-> (select-keys config [:parser :remotes])
                  (assoc :reconciler this
                         :state app-state
                         :db @app-state))
          ;; Update the old environment with the latest layers.
          db (latest-db-from-layers! this env)]
      (assoc env :state (atom db) :db db)))
  p/ILayers
  (add-layer! [this layer]
    (swap! state #(-> (update % :layers layer/add-layer layer)
                      (update :t (fnil inc 0)))))
  (replace-layer! [this layer-id with-layer]
    (let [layers-update
          ;; Remove snapshots that are based on remote layers.
          ;; TODO: Should we really just remove snapshots
          ;; that are based on layers until this point?
          ;; Should be the same?
          (comp #(layer/replace-layer % layer-id with-layer)
                #(layer/remove-snapshots-after % layer-id))]
      (swap-vals! state #(-> %
                             (update :layers layers-update)
                             (update :t (fnil inc 0))))
      (log "replaced layer: " with-layer)))

  p/IBasis
  (basis-t [this] (:t @state))
  p/IReconciler
  (get-config [_] config)
  (get-config [_ k] (get config k))
  (react-class [this component-spec]
    (if-let [klass (get (:class-cache @state) component-spec)]
      klass
      (let [klass (lajter.react/create-class component-spec)]
        (swap! state update :class-cache assoc component-spec klass)
        klass)))
  (reconcile! [this]
    (swap! state dissoc :scheduled-render?)
    (let [{:keys [parser root-render target indexer root-component]} config

          env (p/to-env this)
          root-class (p/react-class this root-component)
          all-props (parser env (get-full-query this root-class))
          all-routing (get-full-routing this root-class)

          {:keys [root-element]} @state]

      (if (some? root-element)
        (let [prev-props (p/all-clj-props root-element)
              prev-routing (p/all-clj-routes root-element)
              cs (p/components-to-render indexer
                                         all-props all-routing
                                         prev-props prev-routing)
              cs ((:optimize config identity) cs)]
          (doseq [c cs]
            (when (and (p/is-indexed? indexer c)
                       (not= (p/basis-t this) (p/basis-t c)))
              (lajter.react/update-component! this
                                              c
                                              all-props
                                              all-routing))))
        (let [root-elem
              (-> (lajter.react/create-element
                    root-class this 0 all-props all-routing nil)
                  #?(:clj lajter.react/call-render)
                  (root-render target))]
          (swap! state assoc :root-element root-elem)
          root-elem))))

  (schedule-render! [this]
    (let [[old _] (swap-vals! state assoc :scheduled-render? true)]
      (not (:scheduled-render? old))))
  (schedule-sends! [this]
    (let [[old _] (swap-vals! state assoc :scheduled-sends? true)]
      (not (:scheduled-sends? old))))
  (send! [this]
    (log "SEND!: " (dissoc @state :root-element))
    (let [remote-layer (layer/first-remote-unsent (:layers @state))]
      (swap! state #(-> (assoc % :scheduled-sends? nil)
                        (cond-> (some? remote-layer)
                                (update :layers layer/mark-sent-layer remote-layer))))
      (log "Remote-layer: " remote-layer)
      (doseq [target (:layer.remote/targets remote-layer)]
        (let [query (layer/to-remote-query remote-layer target)]
          (let [cb (fn [value]
                     (p/replace-layer! this
                                     (:layer/id remote-layer)
                                     (layer/->merge-layer remote-layer value))
                     (schedule-render! this)
                     (schedule-sends! this))]
            ((:send-fn config) this cb query target))))))
  (select-route [this route-data]
    (when-let [f (:route-fn config)]
      (f (p/to-env this)
         route-data)))
  (query-params [this]
    (when-let [f (:query-param-fn config)]
      (f (p/to-env this)))))

;;;;;;;;;;;;;;;;;
;; Indexer

(defn- component-query-keys [component]
  (:lajter.query/keys (p/spec-map component)))

(defn- routing-keys [component]
  (keys (:lajter/routing (p/spec-map component))))

(defn- dissoc-same [a b]
  (reduce-kv (fn [m k v] (cond-> m (identical? v (get b k)) (dissoc k)))
             a
             a))

(def idx->keys-fn
  {:query-key->component component-query-keys
   :route-key->component routing-keys})

(defrecord Indexer [state]
  p/IIndexer
  (index-component! [this component]
    (let [add-component
          (fn [state idx]
            (reduce #(update-in %1 [idx %2] (fnil conj #{}) component)
                    state
                    ((get idx->keys-fn idx) component)))]
      (swap! state
            #(-> (update % :components (fnil conj #{}) component)
                 (add-component :query-key->component)
                 (add-component :route-key->component)))))
  (drop-component! [this component]
    (let [remove-component
          (fn [state idx]
            (reduce #(update-in %1 [idx %2] (fnil disj #{}) component)
                    state
                    ((get idx->keys-fn idx) component)))]
      (swap! state
            #(-> (update % :components disj component)
                 (remove-component :query-key->component)
                 (remove-component :route-key->component)))))
  (is-indexed? [this component]
    (contains? (:components @state) component))
  (components-to-render [this all-props all-routes prev-props prev-routes]
    (let [state @state
          query-index (:query-key->component state)
          route-index (:route-key->component state)
          changed-props (dissoc-same all-props prev-props)
          by-props (vals (select-keys query-index (keys changed-props)))
          changed-routes (dissoc-same all-routes prev-routes)
          by-routes (vals (select-keys route-index (keys changed-routes)))]
      (-> #{}
          (into cat by-props)
          (into cat by-routes)))))

(defn indexer []
  (Indexer. (atom {})))

;;;;;;;;;;;;;;;;;;;;;;;
;; Public entry point

;; TODO: document what this does:
(def default-route-fn
  (fn [env {:lajter.routing/keys [route choices]}]
    (throw
      (ex-info "Accessed routing but now routing function was provided. Please pass a :route-fn to your reconciler."
               {:fn-key    :route-fn
                :arguments '[env {:lajter.routing/keys [route choices]}]}))))

(defn mount
  ([config root]
   (mount config root nil))
  ([{:keys [parser send-fn merge-fn route-fn
            query-param-fn indexer optimize]
     :or   {send-fn        (fn [reconciler cb query target])
            merge-fn       (fn [reconciler db value tx-info])
            route-fn       default-route-fn
            query-param-fn (fn [env])
            indexer        (indexer)
            optimize       #(sort-by p/depth %)}
     :as   config}
    root target]
   (let [parser (or parser
                    (when (or (:read config) (:mutate config))
                      (lajt.parser/parser
                        (merge-with into
                                    {:lajt.parser/query-plugins [(lajt.parser/dedupe-query-plugin {})]}
                                    config)))
                    (throw
                      (ex-info
                        "No :parser, :read or :mutate passed in config"
                        {:config config})))
         ;; TODO: Add this to reconciler component/start
         layers (-> (layer/init-layers)
                    (layer/add-layer
                      (layer/->snapshot-layer @(:state config))))
         reconciler (->Reconciler
                      (assoc config
                        :root-component root
                        :target target
                        :parser parser
                        :send-fn send-fn
                        :merge-fn merge-fn
                        :route-fn route-fn
                        :query-param-fn query-param-fn
                        :indexer indexer
                        :optimize optimize)
                      (atom {:layers layers}))]
     ;; TODO: Add to lifecycle component/start
     (add-watch (:state config) ::reconciler
                (fn [k ref old-state new-state]
                  (schedule-render! reconciler)))
     reconciler)))
