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

(defn ->react-class [{:keys [display-name render shouldComponentUpdate
                             constructor getDerivedStateFromProps
                             componentDidMount getSnapshotBeforeUpdate
                             componentDidUpdate componentWillUnmount
                             componentDidCatch]}]
  #?(:cljs
     (let [obj #js {:display-name
                    display-name
                    :render
                    (fn []
                      (with-this
                        (render (get-props))))
                    :shouldComponentUpdate
                    (fn [next-props next-state]
                      (with-this
                        (or (not= (get-props) (get-props next-props))
                            (not= (get-state) (get-state next-state)))))}]
       (set-methods! obj
         constructor
         (fn [props]
           (with-this
             (constructor props)))
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
                   (prn "CALLING getDerivedStateFromProps")
                   (with-this
                     (let [old-state (get-state prev-state)
                           derived (getDerivedStateFromProps (get-props next-props) old-state)]
                       (cond
                         (not= old-state derived)
                         #js {:clj$state derived}
                         (nil? prev-state)
                         #js {}))))))
         klass))))

(def react-class
  {:render
   (fn [props]
     [:div "foo"])})

;; Make this trigger. We need a main.

(defonce ExperimentComponent
  (->react-class
    {:displayName "experiment"
     :render      (fn [props]
                    (dom/div nil
                             "Rendering experiment :Dx"
                             (dom/span nil (str "props: " props))
                             (dom/span nil (str "state: " (get-state)))))
     :getDerivedStateFromProps (fn [props]
                                 {:initial-state (:bar (:a props))})}))

(defn experiment []
  #?(:cljs
     (let [elem (react/createElement ExperimentComponent
                                     #js {:clj$props {:foo [1 2 3] :bar {:a :b}}})]
       (log elem)
       (log (.getElementById js/document "app"))
       (let [app-div (.getElementById js/document "app")]
         (react-dom/render elem app-div))

       )))

(defn reloaded []
  (log "RELOADED :D")

  (experiment))
