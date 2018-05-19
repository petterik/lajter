(ns lajter.core
  (:require
    [lajter.react]
    [lajter.logger :refer [log]]
    [lajter.protocols :as p]
    [lajter.history :as history]
    [lajt.parser]
    [om.dom :as dom]
    #?@(:cljs [[react :as react]
               [react-dom :as react-dom]
               [goog.object :as gobj]])))

#?(:cljs
   (do
     (defn- get-js-prop [obj k]
       (some-> (or (.-props obj) obj) (gobj/get k)))

     (extend-type object
       p/IReactElement
       (clj-props [this]
         (get-js-prop this "lajter$clj-props"))
       (clj-state [this]
         (some-> (or (.-state this) this)
                 (gobj/get "lajter$clj-state")))
       p/IHasReconciler
       (get-reconciler [this]
         (get-js-prop this "lajter$reconciler"))
       p/IQuery
       (query [this]
         (some-> (get-js-prop this "lajter$react-class")
                 (p/query))))))

(defn reconciler? [x]
  (satisfies? p/IReconciler x))

(defn component? [x]
  (and (instance? p/IReactElement x)
       (some? (lajter.react/clj-props x))))

(defn get-query
  ([] (p/query lajter.react/*this*))
  ([x] (p/query x)))

(defn get-full-query
  ([]
   (get-full-query (p/get-reconciler lajter.react/*this*)))
  ([reconciler]
   (get-query reconciler)))

(defn ->react-class [spec]
  (lajter.react/create-class spec))

;; Make this trigger. We need a main.

(def experiment-component-map
  {:query
   [:foo {:bar [:a]}]
   :displayName
   "experiment"
   :render
   (fn [props]
     (dom/div nil
       (dom/span nil (str "props: " props))
       (dom/span nil (str " state: " (lajter.react/clj-state)))))
   :getDerivedStateFromProps
   (fn [props state]
     (log "in derived state. Props: " props)
     {:initial-state (get-in props [:bar :a])})})

(def ExperimentComponent
  (->react-class experiment-component-map))

(log ExperimentComponent)

(defn create-instance [reconciler klass props]
  #?(:cljs
     (react/createElement klass #js {:lajter$clj-props props
                                     :lajter$reconciler reconciler
                                     :lajter$react-class klass})))

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

(defn transact!
  ([query] (transact! lajter.react/*this* query))
  ([x query]
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
      (schedule-sends! reconciler))))

(defn mount []
  #?(:cljs
     (let [app-state (atom {:foo [1 2 3 4]
                            :bar {:a :b}})
           send-fn (fn [target query])
           reconciler-state (atom {})
           target (.getElementById js/document "app")
           root ExperimentComponent
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
           r (reify
               IStoppable
               (stop! [this]
                 (remove-watch app-state ::reconciler))
               IEnvironment
               (to-env [this]
                 {:state app-state
                  :parser parser})
               p/IQuery
               (query [_] (p/query root))
               p/IReconciler
               (reconcile! [this]
                 (swap! reconciler-state dissoc :scheduled-render?)
                 (let [props (parser (to-env this) (get-query root))]
                   (react-dom/render (create-instance this root props) target)))
               (schedule-render! [this]
                 (let [[old _] (swap-vals! reconciler-state assoc :scheduled-render? true)]
                   (not (:scheduled-render? old))))
               (schedule-sends! [this]
                 (let [[old _] (swap-vals! reconciler-state assoc :scheduled-sends? true)]
                   (not (:scheduled-sends? old))))
               (queue-sends! [this remote-target query]
                 (swap! reconciler-state update-in
                        [:queued-sends remote-target]
                        (fnil into []) query))
               (send! [this]
                 (let [[old _] (swap-vals! reconciler-state assoc
                                           :queued-sends {}
                                           :scheduled-sends? nil)]
                   (doseq [[target query] (:queued-sends old)]
                     (send-fn this target query)))))]
       (add-watch app-state ::reconciler
                  (fn [k ref old-state new-state]
                    (schedule-render! r)))
       r)))

(defonce reconciler-atom (atom nil))
(defn redef-reconciler []
  (when-let [r @reconciler-atom]
    (stop! r))
  (reset! reconciler-atom (mount)))

(defn experiment []
  #?(:cljs
     (let [r @reconciler-atom
           elem (create-instance r ExperimentComponent {:foo [1] :bar {:a "test"}})]
       (log "elem: " elem)
       (log "query works? "
            (= (:query experiment-component-map)
               (get-query ExperimentComponent)
               (get-query elem)))

       (log "env: " (to-env r))
       #_(swap! (:state (to-env r)) update :foo pop)
       )))

(defn reloaded []
  (log "RELOADED :D")
  (when (or (nil? @reconciler-atom))
    (redef-reconciler))

  (schedule-render! @reconciler-atom)

  (experiment))
