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
  (let [query-gen (->> (gen/tuple (gen-mutates "remote")
                                  (gen-mutates "local")
                                  (gen-reads "remote")
                                  (gen-reads "local"))
                       (gen/fmap #(into [] cat %)))]

    (gen/bind (s/gen (s/nilable map?))
              (fn [params]
                (cond->> query-gen
                         (some? params)
                         (gen/fmap (fn [query]
                                     (if (empty? query)
                                       query
                                       (list query params)))))))))

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
   :layer/query-params   (second (parser/separate-query-from-params query))
   :layer/query          query})

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

(s/def ::query-mutates (s/coll-of ::parser/mutation-expr :kind vector?))

(defn gen-local-layer []
  (gen/hash-map :layer.local/mutates (s/gen ::query-mutates)))

(defn gen-merge-layer []
  (gen/hash-map :layer.merge/value (s/gen map?)))

(s/def ::set-of-keys (s/coll-of keyword? :kind set?))

(defn gen-remote-layer []
  (->> (gen/hash-map
         :layer.remote/keys (s/gen ::set-of-keys)
         :layer.remote/mutates (s/gen ::query-mutates)
         :layer.remote/reads (s/gen ::parser/read-exprs)
         :layer.remote/targets (s/gen ::set-of-keys))
       (gen/such-that #(every? (comp seq val) %))))

(defn gen-snapshot-layer []
  (gen/hash-map :layer.snapshot/db (s/gen map?)))

(defn gen-layer-ids []
  (gen/vector-distinct (s/gen pos-int?)))

(defn gen-layers
  ([] (gen-layers #{}))
  ([exclusions]
   (-> (gen/one-of
         (into []
               (comp (remove (set exclusions))
                     (map (fn [gen] (gen))))
               [gen-local-layer
                gen-merge-layer
                gen-snapshot-layer
                gen-remote-layer]))
       (gen/vector)
       (gen/bind (fn [layers]
                   (gen/fmap
                     (fn [ids]
                       (map #(layer/with-id % %2) layers ids))
                     (gen/such-that #(<= (count layers) (count %))
                                    (gen-layer-ids)
                                    100)))))))

(defn gen-take [n gen]
  (gen/fmap (partial take n)
            (gen/such-that #(<= n (count %)) gen 100)))

(defn one [gen]
  (gen/fmap first (gen/not-empty gen)))

(defn at-least [n gen]
  (gen/such-that #(<= n (count %)) gen))

(defn naive-remove-snapshots [layers]
  (remove #(contains? % :layer.snapshot/db) layers))

(tc.test/defspec remote-snapshot-after=>removes-snapshots-after-a-certain-layer
  30
  (prop/for-all [layers (gen-layers)
                 layer (one (gen-layers #{gen-snapshot-layer}))]
    (let [layer (layer/with-id layer "non-colliding-id")]
      (and
        (every? pos-int? (map :layer/id layers))
        (= (concat layers [layer] (naive-remove-snapshots layers))
           (layer/remove-snapshots-after
             (concat layers [layer] layers)
             (:layer/id layer)))))))

(tc.test/defspec top-layers=>layers-until-the-latest-snapshot
  30
  (prop/for-all [layers1 (gen-layers #{gen-snapshot-layer})
                 layers2 (gen-layers #{gen-snapshot-layer})
                 snap1 (gen-snapshot-layer)
                 snap2 (gen-snapshot-layer)]
    (= {:layers (seq layers2) :snapshot snap2}
       (layer/top-layers (vec (concat layers1 [snap1] layers1 [snap2] layers2))))))

(deftest top-layers-examples
  (are [layer1 layer2 snap1 snap2]
       (= {:layers (seq layer2) :snapshot snap2}
          (layer/top-layers (vec (concat layer1 [snap1] layer1 [snap2] layer2))))
    () '({:layer.local/mutates [], :layer/id 1})
    #:layer.snapshot{:db {}} #:layer.snapshot{:db {}}

    () () #:layer.snapshot{:db {}} #:layer.snapshot{:db {}}
    ))

(comment
  (def remove-gem '[({:layer.local/mutates [], :layer/id 2} {:layer.local/mutates [], :layer/id 12} {:layer.local/mutates [], :layer/id 3} {:layer.local/mutates [], :layer/id 4} {:layer.local/mutates [], :layer/id 5} {:layer.snapshot/db {}, :layer/id 1} {:layer.local/mutates [], :layer/id 6} {:layer.local/mutates [], :layer/id 7}) {:layer.local/mutates [], :layer/id 12}])

  (def top-layer-args '[({:layer.local/mutates [], :layer/id 1} {:layer.local/mutates [], :layer/id 2}) (#:layer.snapshot{:db {}} #:layer.snapshot{:db {}})])
  (def layers '[])
  (def snap1 (first (second top-layer-args)))
  (def snap2 (second (second top-layer-args)))
  (-> layers)

  (-> layer/LAYERS)
  (split-with (complement layer/remote-layer?) (rseq (-> layer/LAYERS)))

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
