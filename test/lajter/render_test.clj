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
        config {:root-render *render*
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
              (doto (la/mount config spec)
                (la/omish-transact! query))))
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

;; Reconciler with remote query, send, merge, then render.

(deftest reconcile-send-merge-render-test
  (let [initial-state {:foo 1}
        state (atom initial-state)
        remote-state (agent initial-state)
        remote-parser (lajt.parser/parser {:mutate (fn [env k p]
                                                     (swap! (:state env)
                                                            merge p))
                                           :read (fn [env k p]
                                                   (get @(:state env) k))})
        Root   {:lajter/query [:foo]
                :render (fn [_ _ _] (*div* "root"))}
        config {:root-render *render*
                :state       state
                :remotes     [:remote]
                :read        (fn [env k p]
                               (when (= :foo k)
                                 (if (:target env)
                                   true
                                   (get @(:state env) k))))
                :mutate      (fn [env k p]
                               (when (:target env) true))
                :send-fn     (fn [reconciler cb query target]
                               (lajter.logger/log "Sending..")
                               (send remote-state
                                     (fn [state]
                                       (let [state-atom (atom state)
                                             ret (remote-parser
                                                   {:state state-atom}
                                                   query)]
                                         (cb ret)
                                         @state-atom)))
                               (lajter.logger/log "sent! for query: " query))
                :merge-fn    (fn [reconciler env value]
                               (merge (:db env)
                                      (into {}
                                            (filter (comp keyword? key))
                                            value)))}
        reconciler (doto (la/mount config Root)
                     (la/omish-transact! '[(set-state {:foo 2}) :foo])
                     (p/reconcile!))]
    (is (nil? (agent-error remote-state)))
    (await remote-state)
    (let [new-state @(:state (p/to-env reconciler))]
      (lajter.logger/log "LAYERS layers: " (:layers @(:state reconciler)))
      (lajter.logger/log "GOT reconcier state: " new-state)
      (lajter.logger/log "AGAIN!: " (:db (p/to-env reconciler)))
      (is (not= initial-state @remote-state))
      (is (= @remote-state new-state)))))