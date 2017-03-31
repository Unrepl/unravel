(ns unravel.core
  (:require [clojure.string]
            [clojure.pprint :refer [pprint]]
            [lumo.core]
            [lumo.io :refer [slurp]]
            [cljs.reader :refer [read-string]])
  (:import [goog.string StringBuffer]))

(def readline (js/require "readline"))
(def net (js/require "net"))

(def debug? (atom nil))

;; ------

(defn rstrip-one [s]
  (clojure.string/replace s #"\n$" ""))

(defn- reader-eof? [msg]
  (or
   (= "EOF while reading" msg)
   (= "EOF while reading string" msg)))

(defn- read-chars
  [reader]
  (let [sb (StringBuffer.)]
    (loop [c (cljs.reader/read-char reader)]
      (if-not (nil? c)
        (do
          (.append sb c)
          (recur (cljs.reader/read-char reader)))
        (str sb)))))

(defn unblank [s]
  (if (clojure.string/blank? s)
    nil
    s))

(defn safe-read-string [s]
  (let [reader (cljs.reader/push-back-reader s)
        r (try
            (cljs.reader/read reader true ::eof false)
            (catch js/Error e
              (if (reader-eof? (.-message e))
                ::eof
                (throw e))))]
    (when (not= ::eof r)
      [r (unblank (clojure.string/trim (read-chars reader)))])))

;; ------

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
  )

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

(defn did-receive [rl command]
  (dbug "CMD:" (pr-str command))
  (obey command rl))

(defn edn-stream [stream on-read]
  (let [buf (StringBuffer.)]
    (.on stream "readable"
         (fn []
           (when-let [data (.read stream)]
             (.append buf (.toString data "utf8"))
             (when-let [[v rst] (safe-read-string (.toString buf))]
               (on-read v)
               (.clear buf)
               (when rst
                 (.unshift stream (js/Buffer.from rst "utf8")))))))))

(defn consume-until [stream sentinel cb]
  (let [buf (StringBuffer.)
        !on-readable (atom nil)
        on-readable (fn []
                      (when-let [data (.read stream)]
                        (.append buf (.toString data "utf8"))
                        (let [s (.toString buf)
                              idx (.indexOf s sentinel)]
                          (when (not= -1 idx)
                            (.removeListener stream "readable" @!on-readable)
                            (cb)
                            (.unshift stream (js/Buffer.from (subs s idx) "utf8"))))))]
    (reset! !on-readable on-readable)
    (.on stream "readable" on-readable)))

(defn start [host port]
  (doseq [t '[unrepl/ns unrepl/raw unrepl/edn unrepl/param unrepl/... unrepl/object unrepl.java/class
              error]]
    (cljs.reader/register-tag-parser! t identity))
  (let [rl (.createInterface readline #js{:input js/process.stdin
                                          :output js/process.stdout
                                          :prompt ">> "})
        cx (.Socket. net)]
    (doto cx
      (.connect port
                host
                (fn []
                  (.setNoDelay cx true)
                  (.write cx (lumo.io/slurp "scripts/payload.clj"))))
      (.on "error" (fn [err]
                     (println "Socket error:" (pr-str err))
                     (js/process.exit 1)))
      (consume-until "[:unrepl/hello"
                     (fn [] (edn-stream cx (fn [v]
                                             (did-receive rl v))))))
    (.on rl "line" (fn [line]
                     (.write cx
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
