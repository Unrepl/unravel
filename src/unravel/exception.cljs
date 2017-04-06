(ns unravel.exception
  (:require [clojure.string]))

(defn pretty-ex-type [sy]
  (-> sy
      str
      (clojure.string/replace #"^clojure\.lang\.[A-Za-z0-9]+\$|^clojure\.lang\.|^java\.lang\." "")))

(defn print-location [location]
  (if (vector? location)
    (let [[a b c] location]
      (println (str "\t" a "." b " (" c ")")))
    (println (str "\t" (pr-str location)))))

(defn print-ex-line [{:keys [type message] [a b c] :at}]
  (println (str (pretty-ex-type type) " " message "  " a "." b " (" c ")")))

(defn print-ex [{:keys [via trace]}]
  (doseq [line via]
    (print-ex-line line))
  (doseq [location trace]
    (print-location location)))

(defn print-ex-form [e]
  (print-ex (:form e)))
