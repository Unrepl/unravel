(ns unravel.util)

(defn rstrip-one [s]
  (clojure.string/replace s #"\n$" ""))

(defn unblank [s]
  (if (clojure.string/blank? s)
    nil
    s))
