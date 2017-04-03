(ns unravel.core
  (:require [clojure.string]
            [clojure.pprint :refer [pprint]]
            [lumo.core]
            [lumo.io :refer [slurp]]
            [cljs.reader :refer [read-string]]
            [unravel.version :as uv])
  (:import [goog.string StringBuffer]))

(def path (js/require "path"))
(def readline (js/require "historic-readline"))
(def net (js/require "net"))
(def os-homedir (js/require "os-homedir"))

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

(def whitespace-regex #"([\s,])(.*)")
(def word-regex #"([*+!_'?a-zA-Z-][*+!_'?a-zA-Z0-9/.-]*)(.*)")

(defn tokenize [s]
  (loop [pos 0
         s s
         tokens []]
    (if (clojure.string/blank? s)
      tokens
      (if-let [[_ match rst] (re-matches whitespace-regex s)]
        (recur (+ pos (count match)) rst tokens)
        (if-let [[_ word rst] (re-matches word-regex s)]
          (recur (+ pos (count word))
                 rst
                 (conj tokens {:start pos
                               :end (+ pos (count word))
                               :type :word
                               :value word}))
          (recur (inc pos)
                 (subs s 1) (conj tokens {:start pos
                                          :end (inc pos)
                                          :type :syntax
                                          :value (first s)})))))))

(defn find-word-at [s pos]
  (let [tokens (->> s
                    tokenize
                    (filter (comp #{:word} :type)))]
    (:value (or (some (fn [{:keys [type end] :as token}]
                        (when (< pos end)
                          token))
                      tokens)
                (last tokens)))))


;; ------

(defn dbug [& args]
  (when @debug?
    (prn (vec args))))

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

(defmulti process first)

(defmethod process :prompt [[_ opts] rl]
  (let [ns (:form (get opts 'clojure.core/*ns*))]
    (when ns
      (.setPrompt rl (str ns "=> ")))
    (._refreshLine rl)))

(defmethod process :eval [[_ result] rl]
  (cyan #(prn result)))

(defmethod process :exception [[_ e] rl]
  (red #(println (rstrip-one (with-out-str (pprint e))))))

(defmethod process :out [[_ s] rl]
  (.write js/process.stdout s))

(defmethod process :unrepl/hello [command rl])

(defmethod process :default [command rl]
  (println "WARNING: unknown command" (pr-str command)))

(defn did-receive [rl command]
  (dbug :receive command)
  (process command rl))

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

(defn send! [cx s]
  (dbug :send s)
  (.write cx s "utf8"))

(defn action [cx line cursor]
  (when-let [word (find-word-at line (max 0 (dec cursor)))]
    (println)
    (send! cx (str "(do (require 'clojure.repl)(clojure.repl/doc " word "))" "\n"))))

(defn banner [host port]
  (println (str "Unravel " uv/version " connected to " host ":" port "\n"))
  (println "Type ^O for docs of symbol under cursor, ^D to quit"))

(defn resource [path]
  (str (or js/process.env.UNRAVEL_HOME ".")
       "/"
       path))

(defn start* [istream ostream rl host port]
  (let [cx (.Socket. net)]
    (doto cx
      (.connect port
                host
                (fn []
                  (.setNoDelay cx true)
                  (.write cx (-> "scripts/payload.clj" resource lumo.io/slurp))))
      (.on "error" (fn [err]
                     (println "Socket error:" (pr-str err))
                     (js/process.exit 1)))
      (consume-until "[:unrepl/hello"
                     (fn []
                       (banner host port)
                       (edn-stream cx (fn [v]
                                        (did-receive rl v))))))
    (.on rl "line" (fn [line]
                     (.write cx
                             (str line "\n")
                             "utf8")))
    (.on rl "close" (fn []
                      (println)
                      (.exit js/process)))
    (.on istream "keypress"
         (fn [chunk key]
           (when (and (.-ctrl key) (= "o" (.-name key)))
             (action cx (.-line rl) (.-cursor rl)))))))

(defn start [host port]
  (cljs.reader/register-default-tag-parser! tagged-literal)
  (let [istream js/process.stdin
        ostream js/process.stdout
        opts #js{:input istream
                 :output ostream
                 :path (.join path (os-homedir) ".unravel" "history")
                 :maxLength 1000
                 :next #(start* istream ostream % host port)}]
    (.createInterface readline opts)))

(defn fail [message]
  (println message)
  (js/process.exit 1))

(defn print-version! []
  (println "Unravel" uv/version)
  (js/process.exit 0))

(defn parse [args]
  (reduce (fn [acc arg]
            (cond
              (= "--debug" arg)
              (do
                (reset! debug? true)
                acc)
              (= "--version" arg)
              (print-version!)
              :else
              (conj acc arg)))
          []
          args))

(defn -main [& more]
  (let [[host port :as args] (parse more)]
    (when-not (= 2 (count args))
      (fail "Syntax: unravel [--debug] <host> <port>\n        unravel --version"))
    (start host port)))
