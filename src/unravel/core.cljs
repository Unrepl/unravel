(ns unravel.core
  (:require [clojure.string]))

(def readline (js/require "readline"))
(def net (js/require "net"))

(defn accept [v]
  (println "typed in:" (pr-str v)))

(defn connect [on-connect on-data]
  (let [cx (.Socket. net)]
    (doto cx
      (.connect 50505 "localhost"
                (fn []
                  (.setNoDelay cx true)
                  (on-connect)))
      (.on "error" (fn [err] (println "Got error:" err)))
      (.on "data" (fn [data]
                    (on-data (.toString data "utf8")))))))

(defn rstrip-one [s]
  (clojure.string/replace s #"\n$" ""))

(defn start []
  (let [rl (.createInterface readline #js{:input js/process.stdin
                                          :output js/process.stdout
                                          :prompt ">> "})
        client (connect #(.prompt rl)
                        (fn [data]
                          (println (rstrip-one data))
                          (.prompt rl)))]
    (.on rl "line" (fn [line]
                     (.write client
                             (str line "\n")
                             "utf8")))))

(defn -main []
  (start))
