(ns lajter.core
  (:require
    [lajter.react]
    [lajter.layer :as layer]
    [lajter.logger :refer [log]]
    [lajter.protocols :as p]
    [lajter.history :as history]
    [lajter.git :as git]
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

(defprotocol ILayered
  (add-layer! [this layer])
  (replace-layer! [this tx-id with-layer]))

(defn transact! [x query]
  (let [reconciler (p/get-reconciler x)
        tx-id (gen-tx-id!)
        layer (-> (layer/transaction-layer reconciler query)
                  (layer/with-id tx-id))]
    (log "TRANSACT: " query)
    (log "Adding layer: " layer)
    (add-layer! reconciler layer)
    (when (seq (:layer.remote/targets layer))
      (schedule-sends! reconciler))
    (schedule-render! reconciler)))

#_(defn transact! [x query]
  (log " getting reconciler from : " x)
  (let [reconciler (p/get-reconciler x)
        {:keys [history git-atom parser remotes state] :as env} (to-env reconciler)
        #?@(:cljs [tx-id (str (random-uuid))] :clj
                  [tx-id (str (java.util.UUID/randomUUID))])
        db-before-tx (deref state)
        _ (when (some? history)
            (history/add! history tx-id (deref state)))
        ;; Perform mutations
        local-parse (parser (assoc env ::history-id tx-id)
                            query
                            nil)
        remote-parses (into {}
                            (map (juxt identity #(parser env query %)))
                            remotes)
        tx-data {:db-before db-before-tx
                 :db-after  (deref state)
                 :tx-id     tx-id
                 :tx-type  :transact
                 :query     query
                 :remote-queries remote-parses}]
    (when (some? git-atom)
      (swap! git-atom #(-> % (git/add-all tx-data) (git/commit))))
    (p/queue-sends! reconciler tx-data)
    (schedule-sends! reconciler)))

(defn replay-commits [reconciler git commits]
  (comment
    ;; TODO: THIS WILL ALL GO AWAY WHEN THE LAYERS STUFF IS DONE.(?)
    (reduce (fn [git commit]
              (let [{:keys [db-after tx-type query value parser]} (git/content commit)
                    env (assoc (to-env reconciler) :state (atom db-after))
                    #?@(:cljs [tx-id (str (random-uuid))] :clj
                              [tx-id (str (java.util.UUID/randomUUID))])
                    ret (condp = tx-type
                          :transact (parser (assoc env ::history-id tx-id) query)
                          :merge (comment
                                   ;; Do the same thing we did at merge.
                                   ;; We need to unify these stuff.
                                   ;; Build a transaction log that gets executed
                                   ;; and modified (when we need to adjust optimistic
                                   ;; mutations).
                                   ((:merge-fn config) this
                                     (:db-before tx-response)
                                     (:response tx-response)
                                     (dissoc tx-response :db-after))))]
                ))
            git
            commits)))

(defn- db-with-layers [reconciler db layers]
  (let [{:keys [parser] :as env} (p/to-env reconciler)
        mutate-db (fn [db query]
                    (if (empty? query)
                      db
                      (let [state (atom db)]
                        (parser (assoc env :state state :db db) query)
                        @state)))
        merge-db (fn [db to-merge]
                   ((:merge-fn (:config reconciler)) reconciler db to-merge nil))
        apply-layer (fn [db layer]
                      ;; If the layer contains something to merge.
                      ;; Apply the local mutations then call merge.
                      (if-let [to-merge (not-empty (:layer.merge/value layer))]
                        (-> db
                            (mutate-db (:layer.local/mutates layer))
                            (merge-db to-merge))
                        ;; Otherwise, just call all the mutates.
                        (mutate-db db (:layer/mutates layer))))]
    (reduce apply-layer db layers)))

(defn replace-layer [layers layer-id with-layer]
  (doto
    (into []
          (map (fn [layer]
                 (if (= layer-id (:layer/id layer))
                   with-layer
                   layer)))
          layers)
    (as-> $ (when (= layers $)
              (throw
                (ex-info (str "Unable to find layer-id: " layer-id
                              " in layers.")
                         {:layer-id   layer-id
                          :with-layer with-layer
                          :layers     layers}))))))

(defrecord Reconciler [config state]
  p/IHasReconciler
  (get-reconciler [this] this)
  IStoppable
  (stop! [this]
    (remove-watch (:app-state config) ::reconciler))
  p/IEnvironment
  (to-env [this]
    (select-keys config [:parser :state :remotes]))
  ILayered
  (add-layer! [this layer]
    (swap! state update :layers (fnil conj []) layer))
  (replace-layer! [this layer-id with-layer]
    (swap! state update :layers replace-layer layer-id with-layer))

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
          db (db-with-layers this db layers)
          _ (log "new db: " db)
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
    (let [first-remote (fn [layers]
                         (first
                           (eduction
                             (map-indexed vector)
                             (filter (comp seq :layer.remote/targets second))
                             (remove (comp ::sent? second))
                             (take 1)
                             layers)))
          [old _] (swap-vals! state
                              (fn [state]
                                (let [[idx remote-layer] (first-remote (:layers state))]
                                  (-> state
                                      (assoc :scheduled-sends? nil)
                                      (cond-> (some? remote-layer)
                                              (update-in [:layers idx] assoc
                                                         ::sent? true))))))
          [_ remote-layer] (first-remote (:layers old))]
      (log "Remote-layer: " remote-layer)

      (doseq [target (:layer.remote/targets remote-layer)]
        (let [query (into (get-in remote-layer [:layer.remote/mutates target] [])
                          (get-in remote-layer [:layer.remote/reads target]))]
          (let [cb (fn [value]
                     (replace-layer!
                       this
                       (:layer/id remote-layer)
                       (merge remote-layer (-> (layer/->merge-layer value)
                                               (assoc ::sent? true))))
                     (schedule-render! this)
                     (schedule-sends! this))]
            ((:send-fn config) this cb query target))))))
  (merge! [this tx-response]
    (comment
      ;; THIS GIT STUFF SHOULD GO AWAY
      (let [git @(:git-atom config)
           head (git/head-commit git)
           tx-id (:tx-id tx-response)
           tx-commit (git/some-commit git :tx-id tx-id)
           before-tx (git/parent git tx-commit)
           git (-> git
                   (git/checkout (:git.commit/sha before-tx))
                   (git/branch tx-id))
           merged-db ((:merge-fn config) this
                       (:db-before tx-response)
                       (:response tx-response)
                       (dissoc tx-response :db-after))
           ;; TODO: Run local query with the merged db?
           ;; We've got the whole query. Run it in such a way that if the
           ;; mutation has any of the remotes, it's not executed.
           git (-> git
                   (git/add-all {:db-before (:db-before tx-response)
                                 :db-after  merged-db
                                 :tx-id     tx-id
                                 :tx-type   :merge
                                 :value     (:response tx-response)
                                 :query     (:query tx-response)})
                   (git/commit))
           ;; TODO: Replay all actions after the optimistic mutation.
           commits-before-optimistic (-> git
                                         (git/checkout head)
                                         (git/log nil tx-commit)
                                         (butlast))
           git (replay-commits this git commits-before-optimistic)
           ;; TODO: Reset "master" to be this new branch.
           ]
       ;; TODO: Optimize for when there were no remote mutations
       ;; We don't need to do this git dance when that's the case.
       ;; TODO: Reset the app-state to be the new db.
       ))
    (reset! (:state config)
            ((:merge-fn config) this
              (:db-before tx-response)
              (:response tx-response)
              (dissoc tx-response :db-after)))))
;; Got remote response for optimistic mutation (1).
;; Checkout commit before optimistic mutation (0).
;; Branch.
;; Merge the response with db at that commit (0).
;; Replay all actions after the optimistic mutation (1), which is: [2 and 3].
;; Reset "master" to be this new branch.

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
                        (swap! (:state env) update :foo conj (:x p))
                        'foo/pop
                        (swap! (:state env) update :foo pop)
                        'bar/add
                        (swap! (:state env) update :bar assoc (:k p) (:v p)))))})
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
