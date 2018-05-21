(ns lajter.react
  #?(:cljs (:require-macros
             [lajter.react :refer
              [react-method-wrappers
               with-this]]))
  (:require
    [lajter.protocols :as p]
    [clojure.set :as set]
    #?(:clj [medley.core :as medley])
    #?(:cljs [goog.object :as gobj])
    #?@(:cljs
        [[create-react-class :as create-react-class]
         [react :as react]])))

(def ^:dynamic ^:private *this*)

(def constantly-props (fn [this & _] (p/clj-props this)))
(def constantly-state (fn [this & _] (p/clj-state this)))

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
       (update-clj-state! [this f]
         (.setState this (fn [state]
                           #js {:lajter$clj-state
                                (f (p/clj-state state))}))
         this)
       p/IHasReconciler
       (get-reconciler [this]
         (get-js-prop this "lajter$reconciler"))
       p/IQuery
       (query [this]
         (some-> (get-js-prop this "lajter$react-class")
                 (p/query))))))

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
           (if-not (:fn? sig true)
             `(fn [~sym#] ~sym#)
             `(fn [~sym#]
                (fn ~args#
                  (lajter.react/with-this
                    ~(if (empty? arg-bindings#)
                       `(~sym#)
                       `(let [~@arg-bindings#]
                          ~(if (some? call-with#)
                             `(~sym# lajter.react/*this* ~@call-with#)
                             `(~sym# lajter.react/*this* ~@args#)))))))))))

     (defmacro react-method-wrappers [m]
       (medley/map-vals react-method-wrapper (second m)))))

(def method-wrappers
  (react-method-wrappers
    '{:shouldComponentUpdate    {:args [next-props next-state]}
      :getDerivedStateFromProps {:args [next-props prev-state]}
      :render                   {:args [] :call-with [props state]}
      :getSnapshotBeforeUpdate  {:args [prev-props prev-state]}
      :componentDidUpdate       {:args [prev-props prev-state snapshot]}
      :componentDidMount        {:args []}
      :componentWillUnmount     {:args []}
      :componentDidCatch        {:args [error info]}
      :displayName              {:fn? false}}))

(def static-methods #{:getDerivedStateFromProps})
(def static-protocols #{:query})

(def method-middleware
  {:getDerivedStateFromProps
   (fn [f]
     (fn [this next-props old-state]
       (let [next-state (f this next-props old-state)]
         (when (not= old-state next-state)
           #js {:lajter$clj-state next-state}))))})

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

(defn assoc-obj [obj k v]
  #?(:cljs
     (doto obj
       (gobj/set (name k) v))
     :clj (assoc obj k v)))

(defn implement-protocols [klass spec]
  ;;TODO: unhard-code this
  #?(:cljs
     (specify! klass
       p/IReactClass
       (class-spec [this] spec)
       p/IQuery
       (query [this] (:query spec)))
     ;; TODO: Figure out the clj side.
     :clj klass))

(defn wrap-method [method value]
  (let [wrapper (get method-wrappers method)]
    (if-let [mw (get method-middleware method)]
      (wrapper (mw value))
      (wrapper value))))

(defn create-class [spec]
  (let [initial-obj
        #js {:getInitialState
             (fn [] #js {})
             :shouldComponentUpdate
             (fn [next-props next-state]
               (with-this
                 (or (not= (p/clj-props *this*)
                           (p/clj-props next-props))
                     (not= (p/clj-state *this*)
                           (p/clj-state next-state)))))}
        method-specs (apply dissoc spec
                            (set/union static-methods
                                       static-protocols))
        obj (reduce-kv
              (fn [obj method value]
                (assoc-obj obj method
                           (wrap-method method value)))
              initial-obj
              method-specs)]
    (let [#?@(:cljs [klass (create-react-class obj)]
              :clj  [klass obj])
          klass (reduce-kv
                  (fn [klass method value]
                    (assoc-obj klass method
                               (wrap-method method value)))
                  klass
                  (select-keys spec static-methods))
          klass (implement-protocols klass spec)]
      klass)))

(defn create-instance [reconciler klass props]
  #?(:cljs
     (react/createElement klass #js {:lajter$clj-props props
                                     :lajter$reconciler reconciler
                                     :lajter$react-class klass})))
