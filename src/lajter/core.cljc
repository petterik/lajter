(ns lajter.core
  #?(:cljs
     (:require-macros [lajter.core :refer [with-this set-method! set-methods!]]))
  (:require
    [lajter.logger :refer [log]]
    [lajter.protocols :as p]
    [lajt.parser]
    [om.dom :as dom]
    #?@(:cljs [[react :as react]
               [react-dom :as react-dom]
               [create-react-class :as create-react-class]
               [goog.object :as gobj]])
    ))

(defprotocol IProps
  (-get-props [this]))

(defprotocol IState
  (-get-state [this]))

#?(:cljs
   (extend-type object
     IProps
     (-get-props [this]
       (gobj/get this "clj$props"))
     IState
     (-get-state [this]
       (gobj/get this "clj$state"))))

(def ^:dynamic *this*)
(defn get-props
  ([] (get-props (.-props *this*)))
  ([this]
   (when this
     (-get-props this))))

(defn get-state
  ([] (get-state (.-state *this*)))
  ([this]
   (when this
     (-get-state this))))

#?(:clj
   (defmacro with-this [& body]
     `(cljs.core/this-as ~'this
        (binding [*this* ~'this]
          ~@body))))

#?(:cljs
   (defn wrap-with-this [f]
     (fn [& args]
       (with-this
         (apply f args)))))

#?(:clj
   (defmacro set-method! [sym obj f]
     `(when ~sym
        (set! (~(symbol (str ".-" sym)) ~obj)
              ~f))))

#?(:clj
   (defmacro set-methods! [obj & sym-f-pairs]
     `(do
        ~@(map (fn [[sym f]]
                 `(set-method! ~sym ~obj ~f))
               (partition 2 sym-f-pairs)))))

(defprotocol IQuery
  (query [this])
  (children [this]))

(defn reconciler? [x]
  (satisfies? p/IReconciler x))

(defn component? [x]
  #?(:cljs
     (boolean
       (some-> (.-props x) (gobj/get  "clj$component?" nil)))))

(defn get-reconciler [x]
  {:pre [(or (component? x) (reconciler? x))]}
  #?(:cljs
     (cond-> x
             (component? x)
             (-> .-props (gobj/get "clj$reconciler")))))

(defn get-query [x]
  #?(:cljs
     (query (cond-> x
                    (component? x)
                    (-> .-props (gobj/get "clj$react-class"))))))

(defn ->react-class [{:keys [display-name render shouldComponentUpdate
                             getDerivedStateFromProps
                             componentDidMount getSnapshotBeforeUpdate
                             componentDidUpdate componentWillUnmount
                             componentDidCatch
                             query
                             children]}]
  #?(:cljs
     (let [obj #js {
                    ;; Constructor gets defined by react-create-class
                    ;; and it (seems) to call getInitialState. We return
                    ;; an empty state to initialize it to something
                    ;; to avoid the warning
                    ;; Component: Did not properly initialize state during construction. Expected state to be an object, but it was null.
                    ;; See: https://github.com/reactjs/reactjs.org/issues/796
                    :getInitialState
                    (fn [] #js {})
                    :display-name
                    display-name
                    :render
                    (fn []
                      (with-this
                        (render (get-props))))
                    :shouldComponentUpdate
                    (if (some? shouldComponentUpdate)
                      (wrap-with-this shouldComponentUpdate)
                      (fn [next-props next-state]
                        (with-this
                          (or (not= (get-props) (get-props next-props))
                              (not= (get-state) (get-state next-state))))))}]
       (set-methods! obj
         componentDidMount
         (fn []
           (with-this
             (componentDidMount (get-props))))
         getSnapshotBeforeUpdate
         (fn []
           (with-this
             (getSnapshotBeforeUpdate (get-props) (get-state))))
         componentDidUpdate
         (fn [_ _ snapshot]
           (with-this
             (componentDidUpdate (get-props) (get-state) snapshot)))
         componentWillUnmount
         (fn []
           (with-this
             (componentWillUnmount)))
         componentDidCatch
         (fn [error info]
           (with-this
             (componentDidCatch error info))))

       (let [klass (create-react-class obj)]
         (when getDerivedStateFromProps
           ;; Is this nocollapse needed? for this known static methods?
           ;; I've gotten this idea from om.next.
           #_(js* "/** @nocollapse */")
           (set! (.-getDerivedStateFromProps klass)
                 (fn [next-props prev-state]
                   (with-this
                     (let [old-state (get-state prev-state)
                           derived (getDerivedStateFromProps (get-props next-props)
                                                             old-state)]
                       (when (not= old-state derived)
                         #js {:clj$state derived}))))))
         (specify! klass
           IQuery
           (query [this] query)
           (children [this] children))
         klass))))

(def react-class
  {:render
   (fn [props]
     [:div "foo"])})

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
              (dom/span nil (str " state: " (get-state)))))
   :getDerivedStateFromProps
   (fn [props]
     {:initial-state (get-in props [:bar :a])})})

(def ExperimentComponent
  (->react-class experiment-component-map))

(log ExperimentComponent)

(defn create-instance [reconciler klass props]
  #?(:cljs
     (react/createElement
       klass
       #js {:clj$props       props
            :clj$component?  true
            :clj$reconciler  reconciler
            :clj$react-class ExperimentComponent})))

;; TODO: Replace with component's protocol.
(defprotocol IStoppable
  (stop! [this]))

(defprotocol IEnvironment
  (to-env [this]))

(defprotocol IHistory
  (add-history! [this history-id historic-value])
  (get-history [this history-id])
  (get-most-recent-id [this]))

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
  ([query] (transact! *this* query))
  ([x query]
    (let [reconciler (cond-> x (component? x) (get-reconciler))
          {:keys [history parser remotes state] :as env} (to-env reconciler)
          #?@(:cljs [history-id (random-uuid)] :clj
                    [history-id (java.util.UUID/randomUUID)])
          _ (when (some? history)
              (add-history! history history-id (deref state)))
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
     (let [elem (create-instance nil
                  ExperimentComponent
                  {:foo [1] :bar {:a "test"}})
           r @reconciler-atom]
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

  (experiment))
