(ns unravel.loop
  (:require [clojure.string]
            [clojure.pprint :refer [pprint]]
            [lumo.core]
            [lumo.io :refer [slurp]]
            [cljs.reader :refer [read-string]]
            [unravel.version :as uv]
            [unravel.node :as un]
            [unravel.network :as uw]
            [unravel.terminal :as ut]
            [unravel.tags :as ug]
            [unravel.log :as ud]
            [unravel.util :as uu]
            [unravel.lisp :as ul]
            [unravel.exception :as ue]))

(def start-cmd "(unrepl.repl/start)")

(defn send-command [ctx s]
  (uw/send! (:conn-out ctx) (:eval-counter ctx) s))

(defn send-aux-command [ctx s]
  (uw/send! (:aux-out ctx) (:eval-counter ctx) s))

(defmulti process (fn [command origin ctx] [origin (first command)]))

(defmethod process [:conn :prompt] [[_ opts] origin {:keys [rl]}]
  (let [ns (:form (get opts 'clojure.core/*ns*))]
    (when ns
      (.setPrompt rl (if (ut/interactive?)
                       (str ns "=> ")
                       "")))
    (.prompt rl true)))

(defmethod process [:conn :eval] [[_ result counter] origin {:keys [rl eval-handlers]}]
  (let [f (-> @eval-handlers (get counter))]
    (if f
      (f result)
      (ut/cyan #(prn result)))))

(defmethod process [:aux :eval] [[_ [eval-id v] counter] origin {:keys [rl callbacks]}]
  (when-let [f (-> @callbacks (get eval-id))]
    (f v)))

(defmethod process [:conn :exception] [[_ e] origin {:keys [rl]}]
  (ut/red #(println (uu/rstrip-one (with-out-str (ue/print-ex-form (:ex e)))))))

(defmethod process [:conn :out] [[_ s] origin {:keys [rl]}]
  (.write js/process.stdout s))

(defmethod process :default [command]
  (ud/dbug :unknown-command command))

(defn did-receive [ctx command origin]
  (ud/dbug :receive command)
  (process command origin ctx))

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

(defn do-doc [ctx line cursor]
  (when-let [word (ul/find-word-at line (max 0 (dec cursor)))]
    (println)
    (send-command ctx (str (cmd-doc word)))))

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
      (conj start-cmd)
      (clojure.string/join)))

(defn special [{:keys [conn-out eval-counter rl]} cmd]
  (cond
    (or (= "help" cmd))
    (do
      (help)
      (.prompt rl))

    (or (nil? cmd) (re-matches #"^\d*$" cmd))
    (if-let [cmd (get @ug/ellipsis-store (or (some-> cmd js/parseInt) @ug/ellipsis-counter))]
      (uw/send! conn-out eval-counter (str cmd))
      (.prompt rl))))

(defn connect [conn host port full? terminating?]
  (-> (doto conn
        (.connect port
                  host
                  (fn []
                    (when-not @terminating?
                      (.setNoDelay conn true)
                      (ud/dbug :connect full?)
                      (.write conn (if full?
                                     (read-payload)
                                     start-cmd))
                      (.write conn "\n"))))
        (.on "error" (fn [err]
                       (println "Socket error:" (pr-str err))
                       (js/process.exit 1))))
      (.pipe (uw/make-skip "[:unrepl/hello"))
      (.pipe (uw/make-edn-stream))))

(defn call-remote [{:keys [rl callbacks] :as ctx} form cb]
  (let [eval-id (str (gensym))]
    (swap! callbacks
           assoc
           eval-id
           cb)
    (send-aux-command ctx (pr-str [eval-id form]))))

(defn plausible-symbol? [s]
  (re-matches #"^[\w.]*(/[\w.]+)?$" s))

(defn action [{:keys [rl ostream] :as ctx}]
  (when-let [word (ul/find-word-at (.-line rl) (max 0 (dec (.-cursor rl))))]
    (when (plausible-symbol? word)
      (call-remote ctx
                   (list '->> (list 'clojure.repl/doc (symbol word))
                         'with-out-str
                         ;; '(re-matches (re-pattern "(?is)(.*?\n(.*?\n)?(.*?\n)?).*"))
                         ;; 'second
                         )
                   (fn [result]
                     (when result
                       (let [pos (._getCursorPos rl)
                             newline-count (->> result (re-seq #"\n") count)]
                         (println)
                         (.write ostream (str result))
                         (.moveCursor (js/require "readline")
                                      (.-output rl)
                                      (.-cols pos)
                                      (- (+ newline-count 1))))))))))

(defn start [host port]
  (let [istream js/process.stdin
        ostream js/process.stdout
        eval-handlers (atom {})
        eval-counter (atom 0)
        terminating? (atom false)
        conn-out (.Socket. un/net)
        conn-in (connect conn-out host port true terminating?)]
    (.on conn-in
         "started"
         (fn []
           (let [aux-out (.Socket. un/net)
                 aux-in (connect aux-out host port false terminating?)]
             (ud/dbug :conn)
             (.on aux-in
                  "started"
                  (fn []
                    (ud/dbug :go)
                    (when (ut/interactive?)
                      (banner host port))
                    (.createInterface un/readline
                                      #js{:input istream
                                          :output ostream
                                          :path (un/join-path (un/os-homedir) ".unravel" "history")
                                          :maxLength 1000
                                          :completer (fn [line cb]
                                                       (let [word (or (ul/find-word-at line (count line)) "")
                                                             timeout (fn []
                                                                       (println "\n*** completer timed out ***")
                                                                       (cb nil #js[#js[] word]))
                                                             [cb* timeout*] (uu/once-many cb timeout)]
                                                         (let [counter (uw/send! conn-out eval-counter (str (cmd-complete word)))]
                                                           (js/setTimeout timeout* 3000)
                                                           (swap! eval-handlers assoc counter
                                                                  (fn [result]
                                                                    (cb* nil (clj->js [(map str result) word])))))))
                                          :next (fn [rl]
                                                  (let [ctx {:istream istream
                                                             :ostream ostream
                                                             :eval-handlers eval-handlers
                                                             :eval-counter eval-counter
                                                             :callbacks (atom {})
                                                             :conn-in conn-in
                                                             :conn-out conn-out
                                                             :aux-in aux-in
                                                             :aux-out aux-out
                                                             :rl rl}]
                                                    (.on conn-in "data" #(did-receive ctx % :conn))
                                                    (.on aux-in "data" #(did-receive ctx % :aux))
                                                    (.on rl "line" (fn [line]
                                                                     (.clearLine ostream)
                                                                     (if-let [[_ cmd] (re-matches #"^\s*#__([a-zA-Z0-9]*)?\s*$" line)]
                                                                       (special ctx cmd)
                                                                       (send-command ctx line))))
                                                    (.on rl "close" (fn []
                                                                      (reset! terminating? true)
                                                                      (ud/dbug :end "conn-out")
                                                                      (.end conn-out)
                                                                      (.end aux-out)))
                                                    (.on rl "SIGINT" (fn []
                                                                       (println)
                                                                       (.clearLine rl)
                                                                       (.prompt rl false)))
                                                    (.on istream "keypress"
                                                         (fn [chunk key]
                                                           (action ctx)))))}))))))))
