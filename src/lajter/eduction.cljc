(ns lajter.eduction
  #?(:cljs (:require-macros [lajter.eduction :refer [benchmark]]))
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.test.check :as tc :include-macros true]
    [clojure.test.check.properties :as prop :include-macros true]))

(defn- chunked-sequence [xform s]
  (let [xf (xform conj!)
        rrf #?(:clj (#'clojure.core/preserving-reduced xf)
               :cljs (#'cljs.core/preserving-reduced xf))
        step (fn self [v s]
               (if-not s
                 (persistent! (xf v))
                 (let [c (chunk-first s)
                       ;; Calling unreduced for :clj to get the same
                       ;; behaviour as :cljs
                       #?@(:clj  [chunk (unreduced (.reduce c rrf v))]
                           :cljs [chunk (reduce rrf v c)])]
                   (if (reduced? chunk)
                     (seq (persistent! (xf (deref chunk))))
                     (let [next-chunk (chunk-next s)]
                       (if (zero? (count chunk))
                         (lazy-seq
                           (self v next-chunk))
                         (lazy-cat
                           (persistent! chunk)
                           (self (transient []) next-chunk))))))))]
    (step (transient []) (seq s))))

(deftype Eduction2 [xform coll]
  #?@(:clj
      [clojure.lang.Seqable
       (seq [this]
         (let [s (seq coll)]
           (if (chunked-seq? s)
             (seq (chunked-sequence xform coll))
             #_(iterator-seq (.iterator this))
             (seq (clojure.core/->Eduction xform coll)))))]
      :cljs
      [ISeqable
       (-seq [this]
             (let [s (seq coll)]
               (if (chunked-seq? s)
                 (seq (chunked-sequence xform coll))
                 (seq (sequence xform coll)))))]))

(defn eduction2 [& xforms]
  (Eduction2. (apply comp (butlast xforms)) (last xforms)))

;;;;;;;;;;;;;;;;;
;; Generate

(s/def ::small-ints (into #{} (range 30)))

(defn gen-partition-by []
  (s/gen #{(comp (partition-by even?) cat)}))

(defn gen-take []
  (gen/fmap #(take %) (s/gen (set (range 1000)))))

(defn gen-map []
  (s/gen #{(map inc) (map #(* -1 %))}))

(defn gen-filter []
  (s/gen #{(filter even?)}))

(defn gen-drop []
  (gen/fmap #(drop %) (s/gen ::small-ints)))

(defn gen-mapcat []
  (gen/fmap #(mapcat (fn [x] (repeat % x)))
            (s/gen #{1 2 3})))

;; To make sure we're calling the transducers the same amount of time.
(defn counting-xf []
  (fn [rf]
    (let [counter (atom 0)]
      (fn
        ([] (rf))
        ([acc] (rf (unreduced (rf acc @counter))))
        ([acc res]
         (swap! counter inc)
         (rf acc res))))))

(defn gen-counting-xf []
  (s/gen #{(counting-xf)}))

(defn gen-xforms []
  (gen/not-empty
    (gen/vector
     (gen/one-of
       [(gen-take)
        (gen-partition-by)
        (gen-map)
        (gen-filter)
        (gen-drop)
        (gen-counting-xf)
        (gen-mapcat)]))))

(defn check-eduction2 [n]
  (tc/quick-check
    n
    (prop/for-all [xforms (gen-xforms)
                   i (s/gen ::small-ints)]
      (let [args (concat xforms [(range i)])]
        (= (seq (apply eduction args))
           (seq (apply eduction2 args)))))))

;;;;;;;;;;;;;;;;;
;; Benchmarking

#?(:clj
   (do
     (defmacro if-cljs
       [then else]
       (if (:ns &env) then else))

     (defn clj-benchmark [f runs]
       (let [body# `(dotimes [~'_ ~runs]
                      ((fn [] ~f)))]
         `(do
            ~body#
            (time ~body#))))

     (defmacro benchmark [_ f runs]
       `(if-cljs
          (do
            ;; Warm up:
            (dotimes [~'_ ~runs] ((fn [] ~f)))
            (cljs.core/simple-benchmark [] ~f ~runs))
          ~(clj-benchmark f runs)))))

(defn -main [& args]
  (prn "quick check (= eduction2 clojure.core/eduction")
  (prn (check-eduction2 200))

  (let [coll (seq (into [] (range 100000)))]
    (prn "Benchmarking eduction with chunked-seqs")
    (prn "core/eduction - (first (filter ...))")
    (benchmark []
      (first (eduction (filter (fn [x] (< 80000 x))) coll))
      500)
    (prn "new eduction - (first (filter ...))")
    (benchmark []
      (first (eduction2 (filter (fn [x] (< 80000 x))) coll))
      500)
    (prn "core/eduction - (take 1000) (mapcat range)")
    (benchmark []
      (run! (fn [_])
            (seq (eduction (take 1000) (mapcat range) coll)))
      50)
    (prn "new eduction - (take 1000) (mapcat range)")
    (benchmark []
      (run! (fn [_])
            (seq (eduction2 (take 1000) (mapcat range) coll)))
      50)))

(comment
  ;; clj repl output:
  "quick check (= eduction2 clojure.core/eduction"
  "{:result true, :num-tests 200, :seed 1528128367876}"
  "Benchmarking eduction with chunked-seqs"
  "core/eduction - (first (filter ...))"
  "Elapsed time: 1687.285017 msecs"
  "new eduction - (first (filter ...))"
  "Elapsed time: 637.132507 msecs"
  ;; 1687/637 = 2,6483516484x speed up
  "core/eduction - (take 1000) (mapcat range)"
  "Elapsed time: 1768.182948 msecs"
  "new eduction - (take 1000) (mapcat range)"
  "Elapsed time: 905.295919 msecs"
  ;; 1768/905 = 1,9535911602x speed up

  ;; cljs output
  "quick check (= eduction2 clojure.core/eduction"
  "{:result true, :num-tests 200, :seed 1528128143496}"
  "Benchmarking eduction with chunked-seqs"
  "core/eduction - (first (filter ...))
  [], (first (eduction ... coll)), 500 runs, 8531 msecs"
  "new eduction - (first (filter ...))
  [], (first (eduction2 ... coll)), 500 runs, 4302 msecs"
  ;; 8531/4302 = 1,9830311483x speed up
  "core/eduction - (take 1000) (mapcat range)
  [], (run! (fn [_]) (seq (eduction ... coll))), 50 runs, 11866 msecs"
  "new eduction - (take 1000) (mapcat range)
  [], (run! (fn [_]) (seq (eduction2 ... coll))), 50 runs, 6130 msecs"
  ;; 11866/6130 = 1,935725938x speed up
  )
