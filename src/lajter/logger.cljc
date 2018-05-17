(ns lajter.logger)

(defn log [& args]
  #?(:cljs (.apply (.-log js/console) nil (into-array args))
     :clj (apply prn args)))
