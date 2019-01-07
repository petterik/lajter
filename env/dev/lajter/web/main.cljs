(ns ^:figwheel-hooks lajter.web.main
  (:require
    [datascript.core :as d]
    [devtools.core :as devtools]
    [lajter.core :as la]
    [lajter.logger :refer [log]]
    [lajter.model :as model]
    [lajter.model.db :as db]
    [lajter.model.graphql :as graphql]
    [lajter.model.pull :as pull]
    [lajter.protocols :as p]
    [lajter.web.gql-schema :as gql-schema]
    [react-dom :as react-dom]
    [om.dom :as dom]
    [goog.net.XhrIo :as xhrio]))

(defn button [label f]
  (dom/button #js {:onClick f} (dom/span nil label)))

(def ComponentB
  {:lajter/id    :lajter.web.main/ComponentB
   :lajter/query '[Query
                   [viewer [login]]]
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
   :lajter/query    '[Query
                      [viewer [databaseId]]]
   :lajter/computed (fn [parent]
                      {ComponentB
                       {:update-parent
                        #(la/update-state! parent update :counter inc)}})
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
                      {:initial-state 0
                       :counter       (-> props :query/viewer :user/databaseId)})})

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

(defn dedupe-query [query]
  (let [query-db (model/index-model (model/init-meta-db) query)]
    (into []
          (mapcat (fn self [[e]]
                    (let [sym (:v (first (d/datoms query-db :eavt e :model.node/symbol)))
                          children (d/datoms query-db :avet :model.node/parent e)]
                      (cond-> [sym]
                              (seq children)
                              (conj (into [] (mapcat self) children))))))
          (d/datoms query-db :avet :model.plugin.root-node/root? true))))

(defn ^:after-load runit! []
  (when-let [r @reconciler-atom]
    (la/stop! r)
    (reset! reconciler-atom nil))

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
        #_#_
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
        send-fn (fn [reconciler cb query target]
                  (let [gql-query (graphql/pattern->query query)]
                    (xhrio/send "https://api.github.com/graphql"
                                (fn [response]
                                  (log "Response: " response))
                                "POST"
                                (str "{\"query\":\"" gql-query "\"}")
                                #js {"Authorization"
                                     (str "bearer " gql-schema/github-token)}))
                  (cb {}))
        model-db
        (->> (graphql/parse-schema-str gql-schema/gql-schema)
             (graphql/schema->model)
             (model/index-model (model/init-meta-db [graphql/plugin:graphql])))
        #_#_app-state (d/create-conn (db/datascript-schema model-db))

        config {:root-render    react-dom/render
                :remotes        [:remote]
                :parser         (fn [env query & [target]]
                                  (log "query: " query)
                                  (let [pattern (dedupe-query query)]
                                    (if (some? target)
                                      pattern
                                      {})))
                :state          app-state
                #_#_:read           (lajt.read/->read-fn
                                  (fn [k]
                                    {:custom (fn [env] (get @(:state env) k))
                                     :remote true})
                                  {})
                #_#_:mutate         mutate
                :send-fn        send-fn
                :merge-fn       (fn [env value]
                                  (merge (:db env) value))
                :route-fn       (fn [env {:lajter.routing/keys
                                          [route choices]}]
                                  (get-in @(:state env) [:routing route]))
                :query-param-fn (fn [env]
                                  {:routes (:routing @(:state env))})}
        reconciler (la/mount config Router (.getElementById js/document "app"))]

    (reset! reconciler-atom reconciler)
    (la/schedule-render! reconciler)

    (let [config (:config reconciler)
          this reconciler
          target :remote
          query (dedupe-query (la/get-root-query reconciler))
          cb #(log "CB: " %)
          ]
      (log "MY OWN SEND: " query)
      ((:send-fn config) this cb query target))
    ;; responds with:
    ;; {"data":{"viewer":{"databaseId":715415,"login":"petterik"}}}

    ;; TODO: Actually merge a response.
    ;; TODO: Model mutations.
    ;;       * Remote mutations could be a field on the Mutate type?
    ;;         - '[Mutate [^{:a 1 :b 2} add-foo]] <--- gross?
    ;;       * How do we model local mutations?
    ;;       * How can we do local and remote mutations?
    ;; Remember: Parser is a made up thing by om.next. It could be more
    ;;           than a 2- or 3-arity function.
    ;;           - It could be a set of functions.
    ;;             :local-query
    ;;             :remote-query
    ;;             :local-mutations
    ;;             :remote-mutations
    ;; I think it's time for input argument syntax. >:(
    ))

(defn -main [& args]
  (enable-console-print!)
  (devtools/install!)
  (runit!))

(defonce call-main-once (-main))
(log *ns* " loaded")
