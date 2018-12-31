(ns lajter.logger)

#?(:clj
   (defonce lock (Object.)))

(defn log [& args]
  #?(:cljs (.apply (.-log js/console) nil (into-array args))
     :clj  (locking lock
             (apply prn args))))
