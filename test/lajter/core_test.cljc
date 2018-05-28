(ns lajter.core-test
  (:require
    [lajter.core :as la]
    [lajt.parser :as parser]
    [clojure.test :refer [deftest is are]]
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.test.check.properties :as prop :include-macros true]
    [clojure.test.check.clojure-test :as tc.test :include-macros true]))


(s/def ::remote-target keyword?)
(s/def ::reads-only-query ::parser/read-exprs)
(s/def ::mutations-only-query
  (s/coll-of ::parser/mutation-expr :kind vector? :gen-max 3))

(s/def :layer.remote/reads (s/map-of ::remote-target ::reads-only-query))
(s/def :layer.remote/mutates (s/map-of ::remote-target ::mutations-only-query))
(s/def :layer.local/mutates ::mutations-only-query)
(s/def :layer.merge/value any?)
(s/def :layer/id uuid?)
;; Convenience
(s/def :layer.remote/keys (s/coll-of (s/or :mutate symbol? :read keyword?) :kind set?))
(s/def :layer.remote/targets (s/coll-of ::remote-target :kind set?))

(s/def ::layer-map (s/keys :req [(or :layer.remote/mutates
                                     :layer.local/mutates
                                     :layer.merge/value
                                     :layer.remote/reads)]
                           :opt [:layer/id
                                 :layer.remote/keys
                                 :layer.remote/targets]))

(defn ->merge-layer [value]
  {:layer.merge/value value})

(defn- mutation-key? [k]
  (s/valid? ::parser/mutation-id k))

(defn- read-key? [k]
  (s/valid? ::parser/read-id k))

(defn- split-by [pred coll]
  [(filter pred coll)
   (remove pred coll)])

(defn ->remote-layer [parser remotes env parsed-query]
  (let [[mutations reads]
        (->> parsed-query
             (split-by (comp mutation-key? ::parser/key))
             (map lajt.parser/parsed-query->query)
             (map (fn [query]
                    (into {}
                          (comp
                            (map (juxt identity (partial parser env query)))
                            (remove (comp empty? second)))
                          remotes))))]
    {:layer.remote/reads   reads
     :layer.remote/mutates mutations
     :layer.remote/targets (set (concat (keys reads) (keys mutations)))
     :layer.remote/keys    (into #{}
                                 (comp (mapcat parser/query->parsed-query)
                                       (map ::parser/key))
                                 (concat (vals reads) (vals mutations)))}))

(defn ->local-layer [remote-layer parsed-query]
  (let [remote-mutations (:layer.remote/keys remote-layer)]
    {:layer.local/mutates
     (into []
           (comp (filter (fn [{::parser/keys [key]}]
                           (and (mutation-key? key)
                                (not (contains? remote-mutations key)))))
                 (lajt.parser/parsed-query->query))
           parsed-query)}))

(defn transaction-layer
  ([reconciler tx]
   (transaction-layer (:config reconciler) (la/to-env reconciler) tx))
  ([config env tx]
   (let [{:keys [parser remotes]} config
         parsed-query (lajt.parser/query->parsed-query tx)
         remote-layer (->remote-layer parser remotes env parsed-query)
         local-layer (->local-layer remote-layer parsed-query)]
     (merge remote-layer local-layer))))

;; Tests:

(def parser
  (lajt.parser/parser
    {:read   (fn [env k p]
               (if (:target env)
                 (= "remote" (namespace k))
                 (get @(:state env) k)))
     :mutate (fn [env k p]
               (when (:target env)
                 (= "remote" (namespace k))))}))

(defn- gen-with-params [generator]
  (gen/bind (s/gen (s/map-of keyword? number? :gen-max 2))
            (fn [params]
              (gen/fmap #(list % params) generator))))

(defn- gen-with-query [generator]
  (gen/bind (s/gen ::reads-only-query)
            (fn [query]
              (gen/fmap #(hash-map % query) generator))))

(defn- gen-reads [ns]
  (let [key-gen (gen/fmap #(keyword ns (name %)) (gen/keyword))]
    (gen/vector
      (gen/one-of
        [key-gen
         (gen-with-query key-gen)
         (gen-with-params key-gen)
         (gen-with-params (gen-with-query key-gen))]))))

(defn- gen-mutates [ns]
  (let [sym-gen (gen/fmap #(symbol ns (name %)) (gen/symbol))]
    (gen/vector
      (gen/one-of
        [(gen/fmap list sym-gen)
         (gen-with-params sym-gen)]))))

(defn gen-query []
  (->> (gen/tuple (gen-mutates "remote")
                  (gen-mutates "local")
                  (gen-reads "remote")
                  (gen-reads "local"))
       (gen/fmap #(into [] cat %))))

(tc.test/defspec query-generator-test
  50
  (prop/for-all [query (gen-query)]
    (s/valid? ::parser/query query)))

(def parser (lajt.parser/parser
              {:read   (fn [env k p]
                         (if (:target env)
                           (= "remote" (namespace k))
                           (get @(:state env) k)))
               :mutate (fn [env k p]
                         (when (:target env)
                           (= "remote" (namespace k))))}))

(defn transaction-layer-test-call* [m]
  (transaction-layer {:parser  parser
                      :remotes [:remote]}
                     {:state (atom {})}
                     (into [] cat (vals m))))

(defn transaction-layer-test-relation* [{:keys [remote-mutates remote-reads local-mutates]}]
  {:layer.remote/mutates (cond-> {}
                                 (seq remote-mutates)
                                 (assoc :remote remote-mutates))
   :layer.remote/keys    (set (concat (la/query-keys remote-mutates)
                                      (la/query-keys remote-reads)))
   :layer.remote/reads   (cond-> {}
                                 (seq remote-reads)
                                 (assoc :remote remote-reads))
   :layer.remote/targets (cond-> #{}
                                 (or (seq remote-mutates) (seq remote-reads))
                                 (conj :remote))
   :layer.local/mutates  local-mutates})

(tc.test/defspec transaction-layer-test
  50
  (prop/for-all [m (gen/hash-map :remote-mutates (gen-mutates "remote")
                                 :remote-reads (gen-reads "remote")
                                 :local-mutates (gen-mutates "local")
                                 :local-reads (gen-reads "local"))]
    (= (transaction-layer-test-call* m)
       (transaction-layer-test-relation* m))))


(deftest transaction-layer-test-by-example
  (are [m] (= (transaction-layer-test-call* m)
              (transaction-layer-test-relation* m))
    '{:remote-mutates []
      :remote-reads []
      :local-mutates []
      :local-reads []}
    '{:remote-mutates []
      :remote-reads [(:remote/A {})]
      :local-mutates []
      :local-reads []}
    '{:remote-mutates [(remote/B) (remote/B)]
      :remote-reads []
      :local-mutates []
      :local-reads []}
    '{:remote-mutates []
      :remote-reads [:remote/? :remote/?]
      :local-mutates []
      :local-reads []}))

(comment

  (def parser (lajt.parser/parser
                {:read   (fn [env k p]
                           (if (:target env)
                             (= "remote" (namespace k))
                             (get @(:state env) k)))
                 :mutate (fn [env k p]
                           (when (:target env)
                             (= "remote" (namespace k))))}))

  (parser {} '[:remote/foo :remote/foo] :remote)
  (->remote-layer parser
                  [:remote :stateful]
                  {:state (atom {})}
                  [:local/bar :remote/foo '(foo)])
  (transaction-layer {:parser  parser
                      :remotes [:remote]}
                     {:state (atom {})}
                     '[:local/bar :remote/foo (foo {:bar 1}) (foo {:bar 2})
                       :remote/bar
                       (remote/RKOA)])

  (every? (partial s/valid? ::reads-only-query) (gen/sample (gen-remote-reads)))
  )