(ns lajter.history
  (:refer-clojure :exclude [get]))

(defprotocol IHistory
  (add! [this history-id historic-value])
  (get [this history-id])
  (get-most-recent-id [this]))

(deftype Cache [state size]
  IHistory
  (add! [this id x]
    (let [x' (vary-meta x assoc :client-time #?(:cljs (js/Date.) :clj (java.util.Date.)))
          append-cache #(-> (update % :queue conj id)
                            (assoc-in [:index id] x'))
          trim-cache #(-> (update % :queue pop)
                          (update :index dissoc (first (:queue %))))]
      (swap! state (cond-> append-cache
                           (<= size (count (:queue @state)))
                           (comp trim-cache)))))
  (get [this id]
    (get-in @state [:index id]))
  (get-most-recent-id [this]
    (let [q (:queue @state)]
      (nth q (dec (count q)) nil))))

(defn cache [size]
  (Cache. (atom {:index {}
                 :queue #?(:clj  clojure.lang.PersistentQueue/EMPTY
                           :cljs cljs.core.PersistentQueue.EMPTY)})
          size))