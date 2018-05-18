(ns lajter.core
  #?(:cljs
     (:require-macros [lajter.core :refer [with-this set-method! set-methods!]]))
  (:require
    [lajter.logger :refer [log]]
    [om.dom :as dom]
    #?@(:cljs [[react :as react]
               [react-dom :as react-dom]
               [create-react-class :as create-react-class]
               [goog.object :as gobj]])
    ))

(def foo 1)

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

(defn component? [x]
  #?(:cljs
     (boolean
       (some-> (.-props x) (gobj/get  "clj$component?" nil)))))

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
              "Rendering experiment :Dx"
              (dom/span nil (str "props: " props))
              (dom/span nil (str " state: " (get-state)))))
   :getDerivedStateFromProps
   (fn [props]
     {:initial-state (get-in props [:bar :a])})})

(def ExperimentComponent
  (->react-class experiment-component-map))

(log ExperimentComponent)

(defn create-instance [klass props]
  #?(:cljs
     (react/createElement klass #js {:clj$props       props
                                     :clj$component?  true
                                     :clj$react-class ExperimentComponent})))

(defn experiment []
  #?(:cljs
     (let [elem (create-instance
                  ExperimentComponent
                  {:foo [1 2 3 4] :bar {:a :b}})]
       (log "query works? "
            (= (:query experiment-component-map)
               (get-query ExperimentComponent)
               (get-query elem)))
       (log elem)
       (log (.getElementById js/document "app"))
       (let [app-div (.getElementById js/document "app")]
         (react-dom/render elem app-div))

       )))

(defn reloaded []
  (log "RELOADED :D")

  (experiment))
