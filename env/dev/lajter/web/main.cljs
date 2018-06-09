(ns ^:figwheel-hooks lajter.web.main
  (:require
    [devtools.core :as devtools]
    [lajter.logger :refer [log]]
    [lajter.core :as la]
    [lajter.protocols :as p]
    [react-dom :as react-dom]
    [om.dom :as dom]))

(defn button [label f]
  (dom/button #js {:onClick f} (dom/span nil label)))

(def ComponentB
  {:lajter/id    :lajter.web.main/ComponentB
   :lajter/query [:foo :baz]
   :displayName  "lajter.web.main/ComponentB"
   :render       (fn [this props state]
                   (dom/div nil
                     (dom/p nil " Component B")
                     (dom/p nil (str " Props: " props))
                     (dom/p nil (str " State: " state))
                     (button "Update counter"
                             #(la/update-state! this update :counter (fnil inc 0)))
                     (button "Add to foo"
                             #(la/transact! this `[(foo/conj ~{:x (rand-int 100)})]))
                     (button "Remove from foo"
                             #(la/transact! this '[(foo/pop)]))
                     (button "Assoc to bar"
                             #(la/transact! this `[(bar/assoc ~{:k (rand-int 100)
                                                                :v (rand-int 100)})]))
                     (button "Dissoc from bar"
                             #(la/transact! this `[(bar/dissoc)]))
                     (button "Update parent state"
                             (la/get-computed this :update-parent))))})

(def ComponentA
  {:lajter/id       :lajter.web.main/ComponentA
   :lajter/children [ComponentB]
   :lajter/query    [:foo {:bar [:a]}]
   :lajter/computed {ComponentB
                     (fn [parent]
                       {:update-parent
                        #(la/update-state! parent update :counter inc)})}
   :displayName     "lajter.web.main/ComponentA"
   :render          (fn [this props state]
                      (dom/div
                        nil
                        (dom/p nil (str "props: " props))
                        (dom/p nil (str " state: " state))
                        (dom/p nil (str " counter: " (:counter state)))
                        (button "Update counter"
                                #(la/update-state! this update :counter inc))
                        (la/render-child this ComponentB)))
   :getDerivedStateFromProps
                    (fn [_ props state]
                      {:initial-state (-> props :bar first)
                       :counter       (-> props :foo last)})})

(def NavbarA
  {:lajter/id :lajter.web.main/NavbarA
   :displayName "lajter.web.main/NavbarA"
   :render (fn [this props state]
             (dom/div nil "NavbarA"))})

(def NavbarB
  {:lajter/id :lajter.web.main/NavbarB
   :displayName "lajter.web.main/NavbarB"
   :render (fn [this props state]
             (dom/div nil "NavbarB"))})

(def Router
  {:displayName
   "lajter.web.main/Router"
   :lajter/id
   :lajter.web.main/Router
   :lajter/routing
   {:navbar {:route.navbar/a NavbarA
             :route.navbar/b NavbarB}
    :body   {:route.body/a ComponentA
             :route.body/b ComponentB}}
   :componentDidUpdate
   (fn [this]
     (log "Router props: " (p/clj-props this))
     (log "Router routing: " (p/clj-routes this)))
   :render
   (fn [this props state]
     (dom/div nil
       (dom/p nil "ROUTER")
       (dom/div nil
         (dom/span nil "Set navbar:")
         (button "A" #(la/transact! this `[(route/set {:navbar :route.navbar/a})]))
         (button "B" #(la/transact! this `[(route/set {:navbar :route.navbar/b})])))
       (dom/div nil
         (dom/span nil "Set body:")
         (button "A" #(la/transact! this `[(route/set {:body :route.body/a})]))
         (button "B" #(la/transact! this `[(route/set {:body :route.body/b})])))
       (dom/p nil "NAVBAR: ")
       (la/render-route this :navbar)
       (dom/p nil "BODY: ")
       (la/render-route this :body)))})


(defonce reconciler-atom (atom nil))
(defn redef-reconciler [config]
  (when-let [r @reconciler-atom]
    (la/stop! r))
  (reset! reconciler-atom (la/mount config)))

(defn reloaded [config]
  (log "RELOADED :D")
  (when (or (nil? @reconciler-atom))
    (redef-reconciler config))

  (la/schedule-render! @reconciler-atom))

(defn ^:after-load runit! []
  (let [mutate (fn [env k p]
                 (if (:target env)
                   (when (= "foo" (namespace k))
                     [true :foo])
                   (condp = k
                     'route/set
                     (swap! (:state env) update :routing merge p)
                     'foo/conj
                     (swap! (:state env) update :foo (fnil conj []) (:x p))
                     'foo/pop
                     (swap! (:state env) update :foo
                            #(some-> (not-empty %) pop))
                     'bar/assoc
                     (swap! (:state env) update :bar assoc (:k p) (:v p))
                     'bar/dissoc
                     (swap! (:state env) update :bar #(dissoc % (first (keys %)))))))
        app-state (atom {:routing {:navbar :route.navbar/a
                                   :body   :route.body/a}
                         :foo     [1]
                         :bar     {:a :b}
                         :baz     "foo"})
        remote-state (atom @app-state)
        send-fn (fn [reconciler cb query target]
                  (log "would send query: " query
                       " to target: " target)
                  (js/setTimeout
                    #(let [{:keys [parser] :as env} (p/to-env reconciler)
                           remote-parse
                           (parser (assoc env :state remote-state)
                                   (lajt.parser/update-query
                                     (map (fn [{:lajt.parser/keys [key] :as m}]
                                            (if (= 'foo/conj key)
                                              (update-in m [:lajt.parser/params :x] inc)
                                              m)))
                                     query))]
                       (cb (into {}
                                 (remove (comp symbol? key))
                                 remote-parse)))
                    2000))
        config {:root-component Router
                :root-render    react-dom/render
                :target         (.getElementById js/document "app")
                :remotes        [:remote]
                :state          app-state
                :read           (lajt.read/->read-fn
                                  (fn [k]
                                    {:custom (fn [env] (get @(:state env) k))
                                     :remote true})
                                  {})
                :mutate         mutate
                :send-fn        send-fn
                :merge-fn       (fn [reconciler db value tx-info]
                                  (merge db value))
                :route-fn       (fn [env {:lajter.routing/keys
                                          [route choices]}]
                                  (get-in @(:state env) [:routing route]))
                :query-param-fn (fn [env]
                                  {:routes (:routing @(:state env))})}]

    (reset! reconciler-atom nil)
    (reloaded config)))

(defn -main [& args]
  (enable-console-print!)
  (devtools/install!)
  (runit!))

(defonce call-main-once (-main))
(log *ns* " loaded")
