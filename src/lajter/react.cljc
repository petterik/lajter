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

(def constantly-props (fn [this & _] (p/clj-props this)))
(def constantly-state (fn [this & _] (p/clj-state this)))

#?(:cljs
   (do
     (defn- get-js-prop [obj k]
       (some-> (or (.-props obj) obj) (gobj/get k)))

     (extend-type object
       p/IReactElement
       (raw-clj-props [this]
         (get-js-prop this "lajter$raw-clj-props"))
       (clj-props [this]
         (get-js-prop this "lajter$clj-props"))
       (clj-state [this]
         (some-> (or (.-state this) this)
                 (gobj/get "lajter$clj-state")))
       (clj-computed [this]
         (get-js-prop this "lajter$clj-computed"))
       (update-clj-state! [this f]
         (.setState this (fn [state]
                           #js {:lajter$clj-state
                                (f (p/clj-state state))}))
         this)
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
      :displayName              {:fn? false}}))

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
  (let [ks (atom #{})
        parser (lajt.parser/parser {:read (fn [_ k _]
                                            (swap! ks conj k)
                                            nil)})]
    (parser {} (:lajter/query spec))
    @ks))

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

(def default-methods
  {:shouldComponentUpdate
   (fn [next-props next-state]
     (with-this
       (or (not= (p/clj-props *this*)
                 (p/clj-props next-props))
           (not= (p/clj-state *this*)
                 (p/clj-state next-state)))))})

(defn create-class [class-spec]
  (let [spec (assoc class-spec :lajter.query/keys (query-keys class-spec))
        methods (into default-methods (wrap-spec method-wrappers spec))
        statics (wrap-spec static-wrappers spec)]
    (let [klass (create-react-class methods statics)
          ;; TODO: Un hardcode this.
          #?@(:cljs [klass (doto klass
                             (specify!
                               p/ILajterClass
                               (spec-map [this] spec)))])]
      klass)))

(defn create-instance [reconciler klass props computed]
  (let [clj-props (select-keys props (:lajter.query/keys (p/spec-map klass)))]
    #?(:cljs
       (react/createElement klass #js {:lajter$clj-props     clj-props
                                       :lajter$raw-clj-props props
                                       :lajter$reconciler    reconciler
                                       :lajter$react-class   klass
                                       :lajter$clj-computed  computed}))))
