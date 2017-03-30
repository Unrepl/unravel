(ns unravel.core
  (:require [clojure.string]
            [lumo.core]))

(def readline (js/require "readline"))
(def net (js/require "net"))

(defn accept [v]
  (println "typed in:" (pr-str v)))

(defn connect [host port on-connect on-data]
  (let [cx (.Socket. net)]
    (doto cx
      (.connect port
                host
                (fn []
                  (.setNoDelay cx true)
                  (on-connect)))
      (.on "error" (fn [err] (println "Got error:" err)))
      (.on "data" (fn [data]
                    (on-data (.toString data "utf8")))))))

(defn rstrip-one [s]
  (clojure.string/replace s #"\n$" ""))

(defn start [host port]
  (let [rl (.createInterface readline #js{:input js/process.stdin
                                          :output js/process.stdout
                                          :prompt ">> "})
        client (connect host port
                        #(.prompt rl)
                        (fn [data]
                          (println (rstrip-one data))
                          (.prompt rl)))]
    (.on rl "line" (fn [line]
                     (.write client
                             (str line "\n")
                             "utf8")))
    (.on rl "close" (fn [] (.exit js/process)))))

(defn -main [& [host port :as args]]
  (assert (= 2 (count args))
          "Syntax: scripts/run <host> <port>")
  (start host port))
