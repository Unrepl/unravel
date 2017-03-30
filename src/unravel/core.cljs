(ns unravel.core
  (:require [clojure.string]
            [clojure.pprint :refer [pprint]]
            [lumo.core]
            [lumo.io :refer [slurp]]
            [cljs.reader :refer [read-string]]))

(def readline (js/require "readline"))
(def net (js/require "net"))

(def debug? (atom nil))

(defn dbug [& args]
  (when @debug?
    (apply println args)))

(defn tred []
  (.write js/process.stdout "\33[31m"))

(defn tcyan []
  (.write js/process.stdout "\33[36m"))

(defn treset []
  (.write js/process.stdout "\33[0m"))

(defn red [f]
  (tred)
  (f)
  (treset))

(defn cyan [f]
  (tcyan)
  (f)
  (treset))

(defn accept [v]
  (println "typed in:" (pr-str v)))

(defn connect [host port on-connect on-data]
  (let [cx (.Socket. net)]
    (doto cx
      (.connect port
                host
                (fn []
                  (.setNoDelay cx true)
                  (on-connect cx)))
      (.on "error" (fn [err]
                     (println "Socket error:" (pr-str err))
                     (js/process.exit 1)))
      (.on "data" (fn [data]
                    (on-data (.toString data "utf8")))))))

(defn rstrip-one [s]
  (clojure.string/replace s #"\n$" ""))

(defmulti obey first)

(defmethod obey :prompt [command rl]
  (.prompt rl))

(defmethod obey :eval [[_ result] rl]
  (cyan #(prn result)))

(defmethod obey :exception [[_ e] rl]
  (red #(println (rstrip-one (with-out-str (pprint e))))))

(defmethod obey :out [[_ s] rl]
  (.write js/process.stdout s))

(defmethod obey :unrepl/hello [command rl])

(defmethod obey :default [command rl]
  (println "WARNING: unknown command" (pr-str command)))

(defn did-receive [rl data]
  (let [command (cljs.reader/read-string data)]
    (dbug "CMD:" (pr-str command))
    (obey command rl)))

(defn start [host port]
  (doseq [t '[unrepl/ns unrepl/raw unrepl/edn unrepl/param unrepl/... unrepl/object unrepl.java/class
              error]]
    (cljs.reader/register-tag-parser! t identity))
  (let [rl (.createInterface readline #js{:input js/process.stdin
                                          :output js/process.stdout
                                          :prompt ">> "})
        ready? (atom false)
        client (connect host port
                        (fn [cx]
                          (.write cx (lumo.io/slurp "scripts/payload.clj")))
                        (fn [data]
                          (cond

                            (.startsWith data "[:unrepl/hello")
                            (do
                              (reset! ready? true)
                              (did-receive rl data))

                            @ready?
                            (did-receive rl data))))]
    (.on rl "line" (fn [line]
                     (.write client
                             (str line "\n")
                             "utf8")))
    (.on rl "close" (fn [] (.exit js/process)))))

(defn -main [& args]
  (let [[host port :as args] (if (= "--debug" (first args))
                               (do
                                 (reset! debug? true)
                                 (rest args))
                               args)]
    (assert (= 2 (count args))
            "Syntax: scripts/run [--debug] <host> <port>")
    (start host port)))
