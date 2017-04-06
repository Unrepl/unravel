(ns unravel.core
  (:require [clojure.string]
            [clojure.pprint :refer [pprint]]
            [lumo.core]
            [lumo.io :refer [slurp]]
            [cljs.reader :refer [read-string]]
            [unravel.version :as uv]
            [unravel.node :as un])
  (:import [goog.string StringBuffer]))

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

;; ------

(defn dbug [& args]
  (when @debug?
    (prn (vec args))))

(defn info [& args]
  (prn (vec args)))

;; ------

(defonce ellipsis-counter (atom 0))

(defonce ellipsis-store (atom {}))

(defrecord Ellipsis [get])

(defn ellipsis [m]
  (map->Ellipsis m))

(extend-protocol IPrintWithWriter
  Ellipsis
  (-pr-writer [v writer _]
    (let [counter (swap! ellipsis-counter inc)]
      (swap! ellipsis-store assoc counter (:get v))
      (write-all writer "#__" counter))))

(defn register-tag-parsers []
  (cljs.reader/register-default-tag-parser! tagged-literal)
  (cljs.reader/register-tag-parser! 'unrepl/... ellipsis))

;; ------

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

(defmethod process :eval [[_ result counter] rl eval-handlers]
  (let [f (-> @eval-handlers (get counter))]
    (if f
      (f result)
      (cyan #(prn result)))))

(defmethod process :fin [[_ result counter] rl eval-handlers done-cb]
  (done-cb))

(defmethod process :exception [[_ e] rl]
  (red #(println (rstrip-one (with-out-str (print-ex-form (:ex e)))))))

(defmethod process :out [[_ s] rl]
  (.write js/process.stdout s))

(defmethod process :unrepl/hello [command rl])

(defmethod process :started-eval [command rl])
(defmethod process :echo [command rl])

(defmethod process :default [command rl]
  (dbug :unknown-command command))

(defn did-receive [rl command eval-handlers done-cb]
  (dbug :receive command)
  (process command rl eval-handlers done-cb))

(defn edn-stream [stream on-read]
  (let [buf (StringBuffer.)
        active? (atom true)
        done-cb (fn []
                  (dbug :done)
                  (reset! active? false))]
    (.on stream "readable"
         (fn []
           (when-let [data (.read stream)]
             (when @active?
               (.append buf (.toString data "utf8"))
               (when-let [[v rst] (safe-read-string (.toString buf))]
                 (on-read v done-cb)
                 (.clear buf)
                 (when rst
                   (.unshift stream (js/Buffer.from rst "utf8"))))))))))

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

(defn send! [cx eval-counter s]
  (dbug :send s)
  (.write cx s "utf8")
  (.write cx "\n" "utf8")
  (swap! eval-counter inc))


;; use qualified symbols in case code is invoked
;; after calling (in-ns 'invalid-ns)

(defn cmd-complete [prefix]
  (list 'clojure.core/let ['prefix prefix]
        '(clojure.core/let [all (clojure.core/all-ns)
                            [_ ns va] (clojure.core/re-matches #"^(.*)/(.*)$" prefix)
                            vars (clojure.core/->> (if ns
                                                     (clojure.core/some->> ns
                                                                           clojure.core/symbol
                                                                           clojure.core/find-ns
                                                                           clojure.core/ns-publics)
                                                     (clojure.core/ns-map clojure.core/*ns*))
                                                   clojure.core/keys)
                            nss (clojure.core/when-not ns
                                  (clojure.core/->> (clojure.core/all-ns)
                                                    (clojure.core/map clojure.core/ns-name)))]
           (clojure.core/->> (clojure.core/concat vars nss)
                             (clojure.core/filter #(clojure.core/-> %
                                                                    clojure.core/str
                                                                    (.startsWith (clojure.core/or va prefix))))
                             clojure.core/sort))))

(defn cmd-doc [word]
  (str "(do (require 'clojure.repl)(clojure.repl/doc " word "))"))

(defn do-doc [cx eval-counter line cursor]
  (when-let [word (find-word-at line (max 0 (dec cursor)))]
    (println)
    (send! cx eval-counter (str (cmd-doc word)))))

(defn banner [host port]
  (println (str "Unravel " uv/version " connected to " host ":" port "\n"))
  (println "Type ^O for docs of symbol under cursor, ^D to quit")
  (println "Enter #__help for help")
  (println))

(defn help []
  (println)
  (println "Type ^O for docs of symbol under cursor, ^D to quit.")
  (println "Lines starting with `#__` are treated as special commands and
interpreted by the REPL client. The following specials are available:

- `#__help` shows a help screen
- `#__1`, `#__2`, `#__3` ...: expand the numberd lazy seq ellipsis
- `#__`: expand the most recent lazy seq ellipsis ")
  (println))

(defn read-payload []
  (-> (->> ["print.clj" "repl.clj"]
           (map #(un/join-path (or js/process.env.UNRAVEL_HOME ".")
                               "src"
                               "unrepl"
                               %))
           (mapv lumo.io/slurp))
      (conj "(unrepl.repl/start)")
      (clojure.string/join)))

(defn special [cx eval-counter rl cmd]
  (cond
    (or (= "help" cmd))
    (do
      (help)
      (.prompt rl))

    (or (nil? cmd) (re-matches #"^\d*$" cmd))
    (if-let [cmd (get @ellipsis-store (or (some-> cmd js/parseInt) @ellipsis-counter))]
      (send! cx eval-counter (str cmd))
      (.prompt rl))))

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

(defn start [host port]
  (let [istream js/process.stdin
        ostream js/process.stdout
        eval-handlers (atom {})
        eval-counter (atom 0)
        cx (.Socket. un/net)
        setup-rl (fn [rl]
                   (edn-stream cx (fn [v done-cb]
                                    (did-receive rl v eval-handlers done-cb)))
                   (.on rl "line" (fn [line]
                                    (if-let [[_ cmd] (re-matches #"^\s*#__([a-zA-Z0-9]*)?\s*$" line)]
                                      (special cx eval-counter rl cmd)
                                      (send! cx eval-counter line))))
                   (.on rl "close" (fn []
                                     (.end cx)))
                   (.on rl "SIGINT" (fn []
                                      (println)
                                      (.clearLine rl)
                                      (._refreshLine rl)))
                   (.on istream "keypress"
                        (fn [chunk key]
                          (cond
                            (and (.-ctrl key) (= "o" (.-name key)))
                            (do-doc cx eval-counter (.-line rl) (.-cursor rl))))))
        opts #js{:input istream
                 :output ostream
                 :path (un/join-path (un/os-homedir) ".unravel" "history")
                 :maxLength 1000
                 :completer (fn [line cb]
                              (let [word (or (find-word-at line (count line)) "")
                                    timeout (fn []
                                              (println "\n*** completer timed out ***")
                                              (cb nil #js[#js[] word]))
                                    [cb* timeout*] (once-many cb timeout)]
                                (let [counter (send! cx eval-counter (str (cmd-complete word)))]
                                  (js/setTimeout timeout* 3000)
                                  (swap! eval-handlers assoc counter
                                         (fn [result]
                                           (cb* nil (clj->js [(map str result) word])))))))
                 :next setup-rl}]
    (doto cx
      (.connect port
                host
                (fn []
                  (.setNoDelay cx true)
                  (dbug :connect)
                  (.write cx (read-payload))
                  (.write cx "\n")))
      (.on "error" (fn [err]
                     (println "Socket error:" (pr-str err))
                     (js/process.exit 1)))
      (consume-until "[:unrepl/hello"
                     (fn []
                       (banner host port)
                       (.createInterface un/readline opts))))))

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

(defn init []
  (register-tag-parsers))

(defn -main [& more]
  (init)
  (let [[host port :as args] (parse more)]
    (when-not (= 2 (count args))
      (fail "Syntax: unravel [--debug] <host> <port>\n        unravel --version"))
    (start host port)))
