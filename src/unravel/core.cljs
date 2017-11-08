(ns unravel.core
  (:require [clojure.string]
            [unravel.log :as ul]
            [unravel.version :as uv]
            [unravel.loop :as uo])
  (:import [goog.string StringBuffer]))

(defn fail [message]
  (println message)
  (js/process.exit 1))

(defn print-version! []
  (println "Unravel" uv/version (str "(Lumo " lumo.core/*lumo-version* ")"))
  (js/process.exit 0))

(defn parse [args]
  (reduce (fn [acc arg]
            (cond
              (= "--debug" arg)
              (do
                (reset! ul/debug? true)
                acc)
              (= "--version" arg)
              (print-version!)
              :else
              (conj acc arg)))
          []
          args))

(defn init [])

(defn parse-args [args]
  (loop [m {}
         [arg nxt :as args] args]
    (cond
      (nil? arg)
      m

      (= "--debug" arg)
      (recur (assoc m :debug? true) (rest args))

      (= "--version" arg)
      (recur (assoc m :version? true) (rest args))

      (= "--blob" arg)
      (do
        (assert (some? nxt) "Needs parameter")
        (recur (assoc m :blob nxt) (rest (rest args))))

      :else
      (recur (update m :positional (fn [ps] (conj (or ps []) arg)))
             (rest args)))))

(defn -main [& more]
  (let [opts (parse-args more)]
    (if (:version? opts)
      (print-version!)
      (do
        (when-not (= 2 (count (:positional opts)))
          (fail "Syntax: unravel [--debug] <host> <port>\n        unravel --version"))
        (let [[host port] (:positional opts)]
          (when (:debug? opts)
            (reset! ul/debug? true))
          (init)
          (uo/start host port))))))
