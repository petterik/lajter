(ns lajter.layer-test
  (:require
    [lajter.layer :as layer]
    [lajter.core :as la]
    [lajt.parser :as parser]
    [clojure.spec.alpha :as s]
    [clojure.test :refer [deftest is are]]
    [clojure.spec.gen.alpha :as gen]
    [clojure.test.check.properties :as prop :include-macros true]
    [clojure.test.check.clojure-test :as tc.test :include-macros true]))

(defn- gen-with-params [generator]
  (gen/bind (s/gen (s/map-of keyword? number? :gen-max 2))
            (fn [params]
              (gen/fmap #(list % params) generator))))

(defn- gen-with-query [generator]
  (gen/bind (s/gen ::layer/reads-only-query)
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

(def parser
  (lajt.parser/parser
    {:read   (fn [env k p]
               (if (:target env)
                 (= "remote" (namespace k))
                 (get @(:state env) k)))
     :mutate (fn [env k p]
               (when (:target env)
                 (= "remote" (namespace k))))}))

(defn transaction-layer-test-call* [query]
  (layer/transaction-layer {:parser  parser
                            :remotes [:remote]}
                           {:state (atom {})}
                           query))

(defn transaction-layer-test-relation*
  [query {:keys [remote-mutates remote-reads local-mutates] :as m}]
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
   :layer.local/mutates  local-mutates
   :layer/mutates        (into []
                               (comp (filter #{:remote-mutates :local-mutates})
                                     (mapcat #(get m %)))
                               (keys m))
   :layer/query          query})

'[(remote/foo) {:query/people []}]

(defn- m->query [m]
  (into [] cat (vals m)))

(tc.test/defspec transaction-layer-test
  50
  (prop/for-all [m (gen/hash-map :remote-mutates (gen-mutates "remote")
                                 :remote-reads (gen-reads "remote")
                                 :local-mutates (gen-mutates "local")
                                 :local-reads (gen-reads "local"))]
    (let [query (m->query m)]
      (= (transaction-layer-test-call* query)
         (transaction-layer-test-relation* query m)))))


(deftest transaction-layer-test-by-example
  (let [base {:remote-mutates []
              :remote-reads []
              :local-mutates []
              :local-reads []}]
    (are [m] (= (transaction-layer-test-call* (m->query m))
                (transaction-layer-test-relation* (m->query m) m))
      base
      (assoc base :remote-reads '[(:remote/A {})])
      (assoc base :remote-mutates '[(remote/B) (remote/B)])
      (assoc base :remote-reads [:remote/? :remote/?]))))

(comment

  (parser {} '[:remote/foo :remote/foo] :remote)
  (layer/->remote-layer parser
                  [:remote :stateful]
                  {:state (atom {})}
                        (parser/query->parsed-query [:local/bar :remote/foo '(foo)]))
  (layer/transaction-layer {:parser  parser
                      :remotes [:remote]}
                     {:state (atom {})}
                     '[:local/bar :remote/foo (foo {:bar 1}) (foo {:bar 2})
                       :remote/bar
                       (remote/RKOA)])

  (every? (partial s/valid? ::layer/reads-only-query) (gen/sample (gen-remote-reads)))

  )
