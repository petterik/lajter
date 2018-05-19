(ns lajter.react
  #?(:cljs (:require-macros
             [lajter.react :refer
              [react-method-wrappers
               with-this]]))
  (:require
    [lajter.protocols :as p]
    [lajter.logger :as logger]
    [clojure.set :as set]
    #?(:clj [medley.core :as medley])
    #?(:cljs [goog.object :as gobj])
    #?(:cljs [create-react-class :as create-react-class])))

(def ^:dynamic *this*)

(defn clj-props
  ([] (p/clj-props *this*))
  ([x] (p/clj-props x)))

(defn clj-state
  ([] (p/clj-state *this*))
  ([x] (p/clj-state x)))

(def constantly-props (fn [& _] (clj-props)))
(def constantly-state (fn [& _] (clj-state)))

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

     (def argument-xforms
       `{~'next-props clj-props
         ~'next-state clj-state
         ~'prev-props constantly-props
         ~'prev-state constantly-state})

     (def argument-generators
       `{~'prev-props constantly-props
         ~'prev-state constantly-state
         ~'props      constantly-props
         ~'state      constantly-state})

     (defn- xform-args [args]
       (mapcat
         (fn [arg]
           `[~arg (~(get argument-xforms arg `identity) ~arg)])
         args))

     (defn- xform-call-with [call-with]
       (mapcat
         (fn [arg]
           (let [generator# (get argument-generators arg)]
             (when (nil? generator#)
               (throw
                 (ex-info
                   (str "Now argument generator exists for argument: " arg)
                   {:arg arg})))
             `[~arg (~generator#)]))
         call-with))

     (def react-method-wrapper
       (fn [sig]
         (let [args# (:args sig)
               call-with# (:call-with sig)
               arg-bindings# (concat (xform-args args#) (xform-call-with call-with#))
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
                             `(~sym# ~@call-with#)
                             `(~sym# ~@args#)))))))))))

     (defmacro react-method-wrappers [m]
       (medley/map-vals react-method-wrapper (second m)))))

(def method-wrappers
  (react-method-wrappers
    '{:shouldComponentUpdate    {:args [next-props next-state]}
      :getDerivedStateFromProps {:args [next-props prev-state]}
      :render                   {:args [] :call-with [props]}
      :componentDidMount        {:args []}
      :getSnapshotBeforeUpdate  {:args [prev-props prev-state]}
      :componentDidUpdate       {:args [prev-props prev-state snapshot]}
      :componentWillUnmount     {:args []}
      :componentDidCatch        {:args [error info]}
      :displayName              {:fn? false}}))

(def static-methods #{:getDerivedStateFromProps})
(def static-protocols #{:query})

(def method-middleware
  {:getDerivedStateFromProps
   (fn [f]
     (fn [next-props old-state]
       (let [next-state (f next-props old-state)]
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
                 (or (not= (clj-props) (clj-props next-props))
                     (not= (clj-state) (clj-state next-state)))))}
        method-specs
        (apply dissoc spec (set/union static-methods
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
          klass (implement-protocols
                  klass
                  (select-keys spec static-protocols))]
      klass)))


