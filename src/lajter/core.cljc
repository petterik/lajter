(ns lajter.core
  (:require
    [lajter.react]
    [lajter.logger :refer [log]]
    [lajter.protocols :as p]
    #?@(:cljs [[goog.object :as gobj]])
    ))

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

(let [counter (atom 0)]
  (defn- gen-inc-id []
    (swap! counter inc)))

;;;;;;;;;;;;;;;;;;;;
;; Layer v2
;;

(defn- layer:transaction-layer [env query]
  (let [remote-queries
        (into {}
              (keep (fn [target]
                      (when-let [query ((:parser env) env query target)]
                        [target query])))
              (:remotes env))]
    {:layer/id             (gen-inc-id)
     :layer/transaction    query
     :layer/remote-queries (not-empty remote-queries)}))

(defn- layer:init []
  {:order []
   :by-id {}})

(defn layer:get-by-id [layers id]
  (get-in layers [:by-id id]))

(defn- layer:add [layers layer]
  (-> layers
      (update :order conj (:layer/id layer))
      (update :by-id assoc (:layer/id layer) layer)))

(defn- layer:merge [layers id layer]
  (update layers [:by-id id] merge layer))

(defn- layer:top-down [{:keys [order by-id] :as layers}]
  (eduction
    (map #(get by-id %))
    (rseq order)))

(defn- layer:bottoms-up [{:keys [order by-id] :as layers}]
  (eduction
    (map #(get by-id %))
    order))

(defn- layer:final?
  ([layer]
   (or (empty? (:layer/remote-queries layer))
       (= (set (keys (:layer/remote-queries layer)))
          (set (keys (:layer/responses layer))))))
  ([layers layer-id]
   (layer:final? (layer:get-by-id layers layer-id))))

(defn- layer:apply [env layers]
  (let [reconciler (:reconciler env)

        mutate-db (fn [db query]
                    (if (empty? query)
                      db
                      (let [state (atom db)
                            parser (:parser env)]
                        (parser (assoc env :state state :db db) query)
                        @state)))
        merge-db (fn [db responses]
                   (reduce (fn [db {:layer.response/keys [query target value]}]
                             ((p/get-config reconciler :merge-fn)
                               (assoc env :state (atom db)
                                          :db db
                                          :target target
                                          :query query)
                               value))
                           db
                           responses))
        apply-layer (fn [db layer]
                      ;; If the layer contains something to merge.
                      ;; Apply the local mutations then call merge.
                      (if-let [responses (not-empty (:layer/responses layer))]
                        (-> db
                            #_(mutate-db (:layer.local/mutates layer))
                            (merge-db responses))
                        ;; Otherwise, just call all the mutates.
                        (-> db
                            #_(mutate-db (:layer/mutates layer)))))]
    (reduce apply-layer (:db env) layers)))

;;;;;;;;;;;;;;;;;;;
;; Snapshots
;;

(defn take-until [pred coll]
  (transduce (halt-when pred (fn [r h] (conj r h)))
             conj
             []
             coll))

(defn snapshot:init []
  [])

(defn snapshot:trim-and-add
  "Adds the snapshot right after its parent.

  Somewhere else we've decided that the specified snapshot has a specific
  parent, so we'll just abide by that."
  [snapshots snapshot]
  (-> (take-until (comp #{(:snapshot/parent snapshot)} :snapshot/id)
                  snapshots)
      (conj snapshot)))

(defn snapshot:top-down [snapshots]
  (rseq snapshots))

(defn snapshot:clean
  "Updates dirty snapshots to clean if their layers has become final."
  [snapshots layers]
  (let [clean-snap
        (fn [snap]
          (cond-> snap
                  (and (:snapshot/dirty? snap)
                       (every? #(layer:final? layers %) (:snapshot/layer-ids snap)))
                  (dissoc :snapshot/dirty?)))]
    ;; Could get sped up by using an "ordered-map" as for the layers.
    ;; As we could then just update the top dirty snapshots.
    (into []
          (map clean-snap)
          snapshots)))

(defn snapshot:capture [snapshots env layers]
  ;; Find the first non-dirty-snapshot.
  ;; (A snapshot is a reference to the old snapshot + new layers).
  ;; Calculate the new snapshot from that point.

  ;; Note: If a dirty snapshot has layer's in it that are now
  ;;       final, it can be made clean. All layers referncing
  ;;       the dirty one, will be invalid.
  (let [snapshots (snapshot:clean snapshots layers)
        clean-snap (first (eduction
                            (drop-while :snapshot/dirty?)
                            (snapshot:top-down snapshots)))
        top-layer-id (first (eduction
                              (map :layer/id)
                              (filter (set (:snapshot/layer-ids clean-snap)))
                              (layer:top-down layers)))
        new-layer-ids (into []
                            (comp (map :layer/id)
                                  (take-while (complement #{top-layer-id})))
                            (layer:top-down layers))
        new-db (layer:apply (assoc env :db (:snapshot/db clean-snap (:db env)))
                            (eduction
                              (map #(layer:get-by-id layers %))
                              new-layer-ids))
        dirty? (some #(not (layer:final? layers %)) new-layer-ids)]

    (cond-> snapshots
            (and (some? new-db)
                 (not (identical? new-db (:snapshot/db clean-snap))))
            ;; TODO: This code hints to us that snapshots and layers should be in
            ;;       datascript. Just have to figure out how to nicely get the order
            ;;       correctly. The reason it hints is that we have a bunch of relations.
            ;;       Relations between snapshots and relations between layers. There are
            ;;       rules whenever something is dirty or clean, and whether a layer is
            ;;       final or not. It'll probably look very nice in datascript.
            (snapshot:trim-and-add {:snapshot/parent    (:snapshot/id clean-snap)
                                    :snapshot/id        (gen-inc-id)
                                    :snapshot/dirty?    dirty?
                                    :snapshot/layer-ids new-layer-ids
                                    :snapshot/db        new-db}))))


(defn transact! [x query]
  (let [reconciler (p/get-reconciler x)
        env (p/to-env reconciler)
        layer (layer:transaction-layer env query)]
    (p/add-layer! reconciler layer)
    (schedule-sends! reconciler)
    (schedule-render! reconciler)))


(defrecord Reconciler [config state]
  p/IHasReconciler
  (get-reconciler [this] this)
  IStoppable
  (stop! [this]
    (some-> (:state config)
            (remove-watch ::reconciler)))
  p/IEnvironment
  (to-env [this]
    (let [app-state (:state config)]
      (-> (select-keys config [:parser :remotes])
          (assoc :reconciler this
                 :state app-state
                 :db @app-state))))
  p/ILayers
  (add-layer! [this layer]
    (swap! state update :layers layer:add layer)
    (p/capture-snapshot! this))
  (merge-layer! [this layer-id with-layer]
    (swap! state update :layers layer:merge layer-id with-layer)
    (p/capture-snapshot! this))
  (capture-snapshot! [this]
    (let [{:keys [layers]} @state
          env (p/to-env this)
          new-state (swap! state update :snapshots snapshot:capture env layers)
          new-db (-> (:snapshots new-state)
                     (snapshot:top-down)
                     (first)
                     :snapshot/db)]
      ;; Will trigger re-render
      (when (some? new-db)
        (reset! (:state config) new-db))))

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

          root-class (p/react-class this root-component)
          all-props (parser (p/to-env this) (get-full-query this root-class))
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
    (swap! state dissoc :scheduled-sends?)
    (log "SEND!: " (dissoc @state :root-element))
    (let [remote-layer (first
                         (eduction
                           (filter (comp seq :layer/remote-queries))
                           (remove :layer/sent)
                           (layer:bottoms-up (:layers @state))))]

      (when (some? remote-layer)
        (log "Remote-layer: " remote-layer)
        (p/merge-layer! this
                        (:layer/id remote-layer)
                        ;; The fact that a layer is sent has nothing
                        ;; to do with the layer itself. It's a
                        ;; reconciler/snapshot concern.
                        ;; Stick this data somewhere else?
                        {:layer/sent true})
        (doseq [[target query] (:layer/remote-queries remote-layer)]
          (let [cb (fn [value]
                     (p/merge-layer! this
                                     (:layer/id remote-layer)
                                     {:layer/responses
                                      {:layer.response/target target
                                       :layer.response/query  query
                                       :layer.response/value  value}})
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
            merge-fn       (fn [env value] (:db env))
            route-fn       default-route-fn
            query-param-fn (fn [env])
            indexer        (indexer)
            optimize       #(sort-by p/depth %)}
     :as   config}
    root target]
   {:pre [(some? parser)]}
    (let [reconciler (->Reconciler
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
                      (atom {:layers    (layer:init)
                             :snapshots (snapshot:init)}))]
     ;; TODO: Add to lifecycle component/start
     (add-watch (:state config) ::reconciler
                (fn [k ref old-state new-state]
                  (schedule-render! reconciler)))
     reconciler)))
