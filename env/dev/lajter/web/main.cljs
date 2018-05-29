(ns ^:figwheel-hooks lajter.web.main
  (:require
    [devtools.core :as devtools]
    [lajter.logger :refer [log]]
    [lajter.core :as la]
    [lajter.react :as re]
    [lajter.protocols :as p]
    [react-dom :as react-dom]
    [om.dom :as dom]))


(def ComponentB
  {:lajter/id    :lajter.web.main/ComponentB
   :lajter/query [:foo :baz]
   :displayName  "lajter.web.main/ComponentB"
   :render       (fn [this props state]
                   (dom/div nil
                     (dom/p nil " Component B")
                     (dom/p nil (str " Props: " props))
                     (dom/p nil (str " State: " state))
                     (dom/button #js {:onClick
                                      #(la/update-state! this update :counter (fnil inc 0))}
                                 (dom/span nil "Update counter"))
                     (dom/button #js{:onClick
                                     #(la/transact! this `[(foo/conj ~{:x (rand-int 100)})])}
                                 (dom/span nil "Add to foo"))
                     (dom/button #js{:onClick
                                     #(la/transact! this '[(foo/pop)])}
                                 (dom/span nil "Remove from foo"))
                     (dom/button #js {:onClick (la/get-computed this :update-parent)}
                                 (dom/span nil "Update parent state"))))})

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
                        (dom/button #js {:onClick
                                         #(la/update-state! this update :counter inc)}
                                    (dom/span nil "Update counter"))
                        (la/render-child this ComponentB)))
   :getDerivedStateFromProps
                    (fn [_ props state]
                      {:initial-state (get-in props [:bar :a])
                       :counter       (-> props :foo last)})})

(defn ^:after-load runit! []
  (let [config {:root-component ComponentA
                :root-render    react-dom/render
                :target         (.getElementById js/document "app")
                :remotes        [:remote]
                :state          (atom {:foo [1 2 3 4]
                                       :bar {:a :b}
                                       :baz "foo"})}]

    (reset! la/reconciler-atom nil)
    (lajter.core/reloaded config)))

(defn -main [& args]
  (enable-console-print!)
  (devtools/install!)
  (runit!))

(defonce call-main-once (-main))
(log *ns* " loaded")
