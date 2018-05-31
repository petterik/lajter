(ns ^:figwheel-hooks lajter.web.main
  (:require
    [devtools.core :as devtools]
    [lajter.logger :refer [log]]
    [lajter.core :as la]
    [lajter.react :as re]
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
   :render (fn [this props state]
             (dom/div nil "NavbarA"))})

(def NavbarB
  {:lajter/id :lajter.web.main/NavbarB
   :render (fn [this props state]
             (dom/div nil "NavbarB"))})

(def Router
  {:lajter/id      :lajter.web.main/Router
   :lajter/routing {:navbar {:route.navbar/a NavbarA
                             :route.navbar/b NavbarB}
                    :body   {:route.body/a ComponentA
                             :route.body/b ComponentB}}
   :render         (fn [this props state]
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

(defn ^:after-load runit! []
  (let [config {:root-component Router
                :root-render    react-dom/render
                :target         (.getElementById js/document "app")
                :remotes        [:remote]
                :state          (atom {:routing {:navbar :route.navbar/a
                                                 :body   :route.body/a}
                                       :foo     [1 2 3 4]
                                       :bar     {:a :b}
                                       :baz     "foo"})}]

    (reset! la/reconciler-atom nil)
    (lajter.core/reloaded config)))

(defn -main [& args]
  (enable-console-print!)
  (devtools/install!)
  (runit!))

(defonce call-main-once (-main))
(log *ns* " loaded")
