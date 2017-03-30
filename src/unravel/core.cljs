(ns unravel.core
  (:require [clojure.string]
            [lumo.core]
            [cljs.reader :refer [read-string]]))

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

(defmulti obey first)

(defmethod obey :prompt [command rl]
  (.prompt rl))

(defmethod obey :eval [[_ result] rl]
  (prn result))

(defmethod obey :out [[_ s] rl]
  (.write js/process.stdout s))

(defmethod obey :unrepl/hello [command rl])

(defmethod obey :default [command rl]
  (println "WARNING: unknown command" (pr-str command)))

(defn did-receive [rl data]
  (let [command (cljs.reader/read-string data)]
    (obey command rl)))

(defn start [host port]
  (doseq [t '[unrepl/ns unrepl/raw unrepl/edn unrepl/param unrepl/... unrepl/object unrepl.java/class]]
    (cljs.reader/register-tag-parser! t identity))
  (let [rl (.createInterface readline #js{:input js/process.stdin
                                          :output js/process.stdout
                                          :prompt ">> "})
        client (connect host port
                        #(.prompt rl)
                        (fn [data]
                          (did-receive rl data)))]
    (.on rl "line" (fn [line]
                     (.write client
                             (str line "\n")
                             "utf8")))
    (.on rl "close" (fn [] (.exit js/process)))))

(defn -main [& [host port :as args]]
  (assert (= 2 (count args))
          "Syntax: scripts/run <host> <port>")
  (start host port))
