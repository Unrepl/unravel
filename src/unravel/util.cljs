(ns unravel.util)

(defn rstrip-one [s]
  (clojure.string/replace s #"\n$" ""))

(defn unblank [s]
  (if (clojure.string/blank? s)
    nil
    s))

(defn once-many [& fs]
  (let [done? (atom false)]
    (map (fn [f] (fn [& args]
                   (when-not @done?
                     (reset! done? true)
                     (apply f args))))
         fs)))

(defn once [f]
  (let [done? (atom false)]
    (fn [& args]
      (when-not @done?
        (reset! done? true)
        (apply f args)))))
