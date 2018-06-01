(ns lajter.react
  #?(:cljs (:require-macros
             [lajter.react :refer
              [react-method-wrappers
               with-this]]))
  (:require
    [lajter.logger]
    [lajt.parser]
    [lajter.protocols :as p]
    [clojure.set :as set]
    #?(:clj [medley.core :as medley])
    #?(:cljs [goog.object :as gobj])
    #?(:cljs [react :as react])))

(def ^:dynamic ^:private *this*)

(deftype LajterProps [props basis-t])

(defn lajter-props [props basis-t]
  (LajterProps. props basis-t))

(def ^:private nil-props (lajter-props nil -1))

(defn unwrap [^LajterProps lajter-props]
  (.-props lajter-props))

(defn props-basis-t [^LajterProps lajter-props]
  (.-basis-t lajter-props))

(defn latest-props
  ([x y]
   (max-key props-basis-t x y))
  ([x y z]
   (latest-props x (latest-props y z))))

(def constantly-props (fn [this & _] (p/clj-props this)))
(def constantly-state (fn [this & _] (p/clj-state this)))

#?(:cljs
   (do
     (defn- get-js-prop [obj k]
       (some-> (or (.-props obj) obj) (gobj/get k)))

     (defn- get-js-state [obj k]
       (some-> (or (.-state obj) obj) (gobj/get k)))

     (defn- props-props [obj]
       (or (get-js-prop obj "lajter$wrapped-props") nil-props))
     (defn- state-props [obj]
       (or (get-js-state obj "lajter$state$wrapped-props") nil-props))

     (defn set-pending-props! [component wrapped]
       (let [state (.-state component)]
         (gobj/set state "lajter$state$wrapped-props" wrapped)))

     (defn- get-unwrapped-props [obj]
       (unwrap
         (latest-props
          (props-props obj)
          (state-props obj))))

     (defn- clear-old-props! [obj]
       (when-some [sp (state-props obj)]
         (when-not (identical? (latest-props sp (props-props obj))
                               sp)
           (gobj/remove obj "lajter$state$wrapped-props"))))

     (extend-type object
       p/IReactElement
       (all-clj-props [this]
         (:all-props (get-unwrapped-props this)))
       (all-clj-routes [this]
         (:all-routes (get-unwrapped-props this)))
       (clj-props [this]
         (:props (get-unwrapped-props this)))
       (clj-routes [this]
         (:routes (get-unwrapped-props this)))
       (clj-state [this]
         (get-js-state this "lajter$clj-state"))
       (clj-computed [this]
         (get-js-prop this "lajter$clj-computed"))
       (update-clj-state! [this f]
         (doto this
           (.setState (fn [state]
                        (gobj/set state "lajter$clj-state"
                                  (f (p/clj-state state)))))))
       (depth [this]
         (get-js-prop this "lajter$depth"))
       p/IHasReconciler
       (get-reconciler [this]
         (get-js-prop this "lajter$reconciler"))
       p/ILajterClass
       (spec-map [this]
         (some-> (get-js-prop this "lajter$react-class")
                 (p/spec-map))))))

#?(:clj
   (do
     (defmacro if-cljs
       [then else]
       (if (:ns &env) then else))

     (defmacro with-this [& body]
       `(if-cljs
          (cljs.core/this-as ~'this
            (binding [lajter.react/*this* ~'this]
              ~@body))
          (do
            ~@body)))

     (defn- xform-args [args]
       (let [argument-xforms `{~'next-props p/clj-props
                               ~'prev-props p/clj-props
                               ~'next-state p/clj-state
                               ~'prev-state p/clj-state}]
         (mapcat
           (fn [arg]
             `[~arg (~(get argument-xforms arg `identity) ~arg)])
           args)))

     (defn- xform-call-with [call-with]
       (let [argument-generators `{~'prev-props constantly-props
                                   ~'props      constantly-props
                                   ~'prev-state constantly-state
                                   ~'state      constantly-state}]
         (mapcat
           (fn [arg]
             (let [generator# (get argument-generators arg)]
               (when (nil? generator#)
                 (throw
                   (ex-info
                     (str "Now argument generator exists for argument: " arg)
                     {:arg arg})))
               `[~arg (~generator# lajter.react/*this*)]))
           call-with)))

     (def react-method-wrapper
       (fn [sig]
         (let [args# (:args sig)
               call-with# (:call-with sig)
               arg-bindings# (concat (xform-args args#)
                                     (xform-call-with call-with#))
               sym# (gensym)]
           (if (:constant? sig)
             `(fn [~sym#] ~sym#)
             `(fn [~sym#]
                (fn ~args#
                  (lajter.react/with-this
                    ~(if (empty? arg-bindings#)
                       `(~sym# lajter.react/*this*)
                       `(let [~@arg-bindings#]
                          ~(if (some? call-with#)
                             `(~sym# lajter.react/*this* ~@call-with#)
                             `(~sym# lajter.react/*this* ~@args#)))))))))))

     (defmacro react-method-wrappers [m]
       (medley/map-vals react-method-wrapper (second m)))))

(def static-wrappers
  (react-method-wrappers
    '{:getDerivedStateFromProps {:args [next-props prev-state]}}))

(def method-wrappers
  (react-method-wrappers
    '{:shouldComponentUpdate    {:args [next-props next-state]}
      :render                   {:args [] :call-with [props state]}
      :getSnapshotBeforeUpdate  {:args [prev-props prev-state]}
      :componentDidUpdate       {:args [prev-props prev-state snapshot]}
      :componentDidMount        {:args []}
      :componentWillUnmount     {:args []}
      :componentDidCatch        {:args [error info]}
      :displayName              {:constant? true}}))

(def method-middleware
  {:getDerivedStateFromProps
   (fn [f]
     (fn [this next-props old-state]
       (let [next-state (f this next-props old-state)]
         (when (not= old-state next-state)
           #js {:lajter$clj-state next-state}))))
   :componentDidUpdate
   (fn [f]
     (fn [this prev-props prev-state snapshot]
       (let [ret (f this prev-props prev-state snapshot)]
         #?(:cljs (clear-old-props! this))
         ret)))
   :componentDidMount
   (fn [f]
     (fn [this]
       (let [reconciler (p/get-reconciler this)
             indexer (-> reconciler :config :indexer)]
         (p/index-component! indexer this)
         (f this))))
   :componentWillUnmount
   (fn [f]
     (fn [this]
       (let [reconciler (p/get-reconciler this)
             indexer (-> reconciler :config :indexer)]
         (p/drop-component! indexer this)
         (f this))))})

(def default-methods
  {:shouldComponentUpdate
   (fn [next-props next-state]
     (with-this
       (or (not= (p/clj-routes *this*)
                 (p/clj-routes next-props))
           (not= (p/clj-props *this*)
                 (p/clj-props next-props))
           (not= (p/clj-state *this*)
                 (p/clj-state next-state)))))
   :componentDidMount
   (fn [this])
   :componentDidUpdate
   (fn [this prev-props prev-state snapshot])
   :getSnapshotBeforeUpdate
   (fn [this prev-props prev-state])
   :componentWillUnmount
   (fn [this])})

#_(s/def ::class-spec
    (s/keys :req-un [::render]
            :opt-un [::shouldComponentUpdate
                     ::getDerivedStateFromProps
                     ::componentDidMount
                     ::getSnapshotBeforeUpdate
                     ::componentDidUpdate
                     ::componentWillUnmount
                     ::componentDidCatch
                     ::display-name]))

(defn- wrap-spec [wrappers spec]
  (let [wrap-method (fn [[method value]]
                      (let [wrapper (get wrappers method)]
                        (if-let [mw (get method-middleware method)]
                          (wrapper (mw value))
                          (wrapper value))))]
    (into {}
          (map (juxt key wrap-method))
          (select-keys spec (keys wrappers)))))

(defn- query-keys
  "Returns the keys of the component's query."
  [spec]
  (into #{}
        (comp
          (lajt.parser/query->parsed-query)
          (map :lajt.parser/key))
        (:lajter/query spec)))

(defn- create-react-class [methods statics]
  #?(:clj
     {:methods methods
      :statics statics}
     :cljs
     (let [set-all! (partial reduce-kv #(doto %1 (gobj/set (name %2) %3)))
           ctor (doto (fn [props]
                        (with-this
                          (.call js/React.Component *this* props)
                          (gobj/set *this* "state" #js {})))
                  (goog/inherits js/React.Component))]
       (set-all! (gobj/get ctor "prototype") methods)
       (set-all! ctor statics)
       ctor)))

(defn create-class [class-spec]
  (let [spec (assoc class-spec :lajter.query/keys (query-keys class-spec))
        methods (wrap-spec method-wrappers (into default-methods spec))
        statics (wrap-spec static-wrappers spec)]
    (let [klass (create-react-class methods statics)
          ;; TODO: Un hardcode this.
          #?@(:cljs [klass (doto klass
                             (specify!
                               p/ILajterClass
                               (spec-map [_] spec)))])]
      klass)))

(defn- select-props [spec props]
  (select-keys props (:lajter.query/keys spec)))

(defn- select-routes [spec routes]
  (select-keys routes (keys (:lajter/routing spec))))

(defn create-instance
  [reconciler klass props computed routes depth]
  (let [spec (p/spec-map klass)
        clj-props (select-props spec props)
        clj-routes (select-routes spec routes)
        wrapped (lajter-props {:all-props  props
                               :all-routes routes
                               :props      clj-props
                               :routes     clj-routes}
                              (p/basis-t reconciler))]
    #?(:cljs
       (react/createElement
         klass
         #js {:lajter$wrapped-props wrapped
              :lajter$reconciler    reconciler
              :lajter$react-class   klass
              :lajter$clj-computed  computed
              :lajter$depth         depth}))))

(defn update-component!
  [reconciler component props routes]
  (let [spec (p/spec-map component)
        clj-props (select-props spec props)
        clj-routes (select-routes spec routes)
        wrapped (lajter-props {:all-props  props
                               :all-routes routes
                               :props      clj-props
                               :routes     clj-routes}
                              (p/basis-t reconciler))]
    #?(:cljs
       (do
         (set-pending-props! component wrapped)
         (.forceUpdate ^js/React.Component component)))))

;; Put props in both props and state.
;; Place an incrementing basis-t in the props
;; In componentWillUpdate: merge pending props and state.
;; - Merge pending means to ...?
