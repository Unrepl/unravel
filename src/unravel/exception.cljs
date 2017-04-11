(ns unravel.exception
  (:require [clojure.string]
            [unravel.log :as ud]))

(defn pretty-ex-type [sy]
  (-> sy
      str
      (clojure.string/replace #"^clojure\.lang\.[A-Za-z0-9]+\$|^clojure\.lang\.|^java\.lang\." "")))

(defn print-location [location]
  (if (vector? location)
    (let [[a b c] location]
      (println (str "\t" a "." b " (" c ")")))
    (println (str "\t" (pr-str location)))))

(defn format-at [at]
  (if (vector? at)
    (let [[a b c] at]
      (str a "." b " (" c ")"))
    "(unknown)"))

(defn print-ex-line [{:keys [type message at]}]
  (println (str (pretty-ex-type type)
                " "
                message
                "  "
                (format-at at))))

(defn print-ex [{:keys [via trace]}]
  (doseq [line via]
    (print-ex-line line))
  (doseq [location trace]
    (print-location location)))

(defn print-ex-form [e]
  (print-ex (:form e)))
