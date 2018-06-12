(ns lajter.render-test
  (:require
    [clojure.test :as t :refer [deftest is are]]
    [lajter.react :as react]
    [lajter.protocols :as p]
    [lajter.core :as la]
    [om.dom :as dom]
    [rum.core :as rum]))

(def render-libs [{:div-fn    #(apply dom/div nil %&)
                   :render-fn (comp str #'dom/render-to-str*)}
                  {:div-fn    #(apply vector :div %&)
                   :render-fn rum/render-html}])
(def ^:dynamic *render*)
(def ^:dynamic *div*)

(defn ->reconciler [{:keys [root-render]
                     :or {root-render *render*}
                     :as config}]
  (reify
    p/IReconciler
    (get-config [_] config)
    (get-config [_ k] (get config k))
    (react-class [this spec]
      (react/create-class spec))
    p/IBasis
    (basis-t [_] -13)))

(defn setup [f]
  (doseq [{:keys [div-fn render-fn]} render-libs]
    (binding [*render* (fn [x & _] (render-fn x))
              *div* div-fn]
      (f))))

(t/use-fixtures :once setup)

(deftest render-components-test
  (let [reconciler (->reconciler {:root-render *render*})
        Child {:render (fn [this props state]
                         (or (la/get-computed this :div)
                             (*div* "child")))}]
    (are [expected spec] (= (*render* expected)
                            (react/render-to-string
                              (react/create-element
                                (react/create-class spec)
                                reconciler 0 {} {} {})))
      (*div* "hello world")
      {:render (fn [this props state]
                 (*div* "hello world"))}

      ;; With child
      (*div* "parent" (*div* "child"))
      {:lajter/children [Child]
       :render          (fn [this props state]
                          (*div* "parent"
                                 (la/render-child this Child)))}

      ;; With state
      (*div* 4711)
      {:getDerivedStateFromProps
               (fn [this props state]
                 {:initial-state 4711})
       :render (fn [this _ state]
                 (*div* (:initial-state state)))}

      ;; Testing computed
      (*div* "from computed")
      {:lajter/computed (fn [this]
                          {Child {:div (*div* "from computed")}})
       :render (fn [this _ _]
                 (la/render-child this Child))})))

(deftest render-with-reconciler-test
  (let [state (atom {:foo    "FOO"
                     :bar    "BAR"
                     :routes {:navbar :navbar/a
                              :body   :body/a}})
        config {:root-render (fn [elem target]
                               (lajter.logger/log "elem: " elem)
                               (*render* elem target))
                :state       state
                :read        (fn [env k p] (get @(:state env) k))
                :mutate      (fn [env k p]
                               (condp = k
                                 'set/routes (swap! (:state env)
                                                    update :routes
                                                    merge p))
                               (lajter.logger/log "state: " @(:state env)))
                :route-fn    (fn [env {:lajter.routing/keys [route choices]}]
                               (-> @(:state env) :routes (get route)))}

        Child {:lajter/query [:bar]
               :render       (fn [this props state]
                               (*div* (or (some-> props first val)
                                          "no props")))}
        Router {:lajter/routing
                {:navbar {:navbar/a Child
                          :navbar/b (assoc Child :lajter/query [:foo])}
                 :body   {:body/a (dissoc Child :lajter/query)}}
                :render
                (fn [this props state]
                  (*div* (la/render-route this :navbar)
                         (la/render-route this :body)))}]
    (are [expected spec query]
         (= (*render* expected)
            (p/reconcile!
              (doto (la/mount (assoc config :root-component spec))
                (la/transact! query))))
      (*div* "hello world")
      {:render (fn [this props state]
                 (*div* "hello world"))}
      []

      ;; With query:
      (*div* "FOO" (*div* "BAR"))
      {:lajter/query    [:foo]
       :lajter/children [Child]
       :render          (fn [this props state]
                          (*div* (:foo props)
                                 (la/render-child this Child)))}
      []

      ;; Routing
      (*div* (*div* "BAR")
             (*div* "no props"))
      Router
      []

      (*div* (*div* "FOO")
             (*div* "no props"))
      Router
      '[(set/routes {:navbar :navbar/b})]

      (*div* (*div* "FOO"))
      Router
      '[(set/routes {:navbar :navbar/b :body :does-not-exist})]
      )))

(deftest update-component-state-test
  (let [elem (react/create-element
               (react/create-class {:render (fn [this _ state]
                                              (*div* (:foo state)))})
               (->reconciler {}) 0 {} {} {})]
    (is (= (*div* nil) (react/call-render elem)))
    (la/update-state! elem assoc :foo "state")
    (is (= (*div* "state") (react/call-render elem)))))