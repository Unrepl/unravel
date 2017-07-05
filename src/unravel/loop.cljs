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

(defn squawk [rl & xs]
  (println)
  (prn (vec xs))
  (.prompt rl true))

(def start-cmd "(do (in-ns 'user) (unrepl.repl/start))")

(defn send-command [ctx s]
  (uw/send! (:conn-out ctx) s))

(defn send-aux-command [ctx s]
  (uw/send! (:aux-out ctx) s))

(defn set-prompt [{:keys [rl state]} ns warn?]
  (.setPrompt rl (if (ut/interactive?)
                   (str (or ns (:ns @state))
                        (when warn? "\33[31m")
                        "=> "
                        (when warn? "\33[0m"))
                   "")))

(defmulti process (fn [command origin ctx] [origin (first command)]))

(defmethod process [:conn :prompt] [[_ opts] _ {:keys [rl state] :as ctx}]
  (let [ns (:form (get opts 'clojure.core/*ns*))]
    (when ns
      (swap! state assoc :ns ns)
      (set-prompt ctx ns false))
    (.prompt rl true)))

(defmethod process [:conn :eval] [[_ result counter] _ {:keys [rl pending-eval]}]
  (reset! pending-eval nil)
  (ut/cyan #(prn result)))

(defmethod process [:conn :started-eval] [[_ {:keys [actions]}] _ {:keys [rl pending-eval]}]
  (reset! pending-eval {:action actions}))

(defmethod process [:aux :eval] [[_ result counter] _ {:keys [rl callbacks]}]
  (when (and (vector? result) (= :unravel/rpc (first result)))
    (let [[_ eval-id v] result]
      (when-let [f (-> @callbacks (get eval-id))]
        (f v)))))

(defmethod process [:conn :exception] [[_ e] _ {:keys [rl pending-eval]}]
  (reset! pending-eval nil)
  (ut/red #(println (uu/rstrip-one (with-out-str (ue/print-ex-form (:ex e)))))))

(defmethod process [:conn :out] [[_ s] _ {:keys [rl]}]
  (.write js/process.stdout s))

(defmethod process :default [command]
  (ud/dbug :unknown-command command))

(defn did-receive [ctx command origin]
  (ud/dbug :receive {:origin origin} command)
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
  (println "Type ^O for full docs of symbol under cursor, ^D to quit")
  (println "Enter #__help for help")
  (println))

(defn help []
  (println)
  (println "Type ^O for full docs of symbol under cursor, ^D to quit.")
  (println "Lines starting with `#__` are treated as special commands and
interpreted by the REPL client. The following specials are available:

- `#__help` shows a help screen
- `#__1`, `#__2`, `#__3` ...: expand the numberd lazy seq ellipsis
- `#__`: expand the most recent lazy seq ellipsis ")
  (println))

(defn read-payload []
  (lumo.io/slurp "resources/unrepl/blob.clj"))

(defn special [{:keys [conn-out rl] :as ctx} cmd]
  (cond
    (or (= "help" cmd))
    (do
      (help)
      (.prompt rl))

    (or (nil? cmd) (re-matches #"^\d*$" cmd))
    (if-let [cmd (get @ug/ellipsis-store (or (some-> cmd js/parseInt) @ug/ellipsis-counter))]
      (send-command ctx (str cmd))
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
  (let [eval-id (str (gensym))
        cmd (pr-str [:unravel/rpc eval-id form])]
    (swap! callbacks
           assoc
           eval-id
           cb)
    (send-aux-command ctx cmd)))

(defn plausible-symbol? [s]
  (re-matches #"^[*+=?!_?a-zA-Z-.]+(/[*+=?!_?a-zA-Z-.]+)?$" s))

(defn cut [s n]
  (or (some-> (some->> s (re-matches #"^(.{67})(.{3}).*$") second) (str "...")) s))

(defn show-doc [{:keys [rl ostream state] :as ctx} full?]
  (when-let [word (ul/find-word-at (.-line rl) (max 0 (dec (.-cursor rl))))]
    (when (plausible-symbol? word)
      (when (or (not= word (:word @state)) full?)
        (swap! state assoc :word word)
        (call-remote ctx
                     (if full?
                       (list '->>
                             (list 'clojure.repl/doc (symbol word))
                             'with-out-str)
                       (list '->> (list 'clojure.repl/doc (symbol word))
                             'with-out-str
                             '(re-matches (re-pattern "(?is)(.*?\n(.*?\n)?(.*?\n)?(.*?\n)?)(.*)$"))
                             'rest))
                     (fn [r]
                       (if full?
                         (do
                           (println)
                           (.clearScreenDown ostream)
                           (println r)
                           (.prompt rl true))
                         (let [[result more] r]
                           (when result
                             (let [pos (._getCursorPos rl)
                                   lines (clojure.string/split-lines (cond-> (clojure.string/trimr result)
                                                                       more
                                                                       (str "...")))]
                               (println)
                               (doseq [line lines]
                                 (.clearLine ostream)
                                 (println (cut line 70)))
                               (.moveCursor (js/require "readline")
                                            (.-output rl)
                                            (.-cols pos)
                                            (- (+ (count lines) 1)))))))))))))

(defn complete [ctx line cb]
  (let [word (or (ul/find-word-at line (count line)) "")
        timeout (fn []
                  (println "\n*** completer timed out ***")
                  (cb nil #js[#js[] word]))
        [cb* timeout*] (uu/once-many cb timeout)]
    (call-remote ctx
                 (cmd-complete word)
                 (fn [completions]
                   (cb* nil (clj->js [(map str completions) word]))
                   (show-doc ctx false)))))

(defn interrupt [{:keys [rl pending-eval] :as ctx}]
  (if @pending-eval
    (some->> @pending-eval :action :interrupt pr-str (send-aux-command ctx))
    (do
      (println)
      (when (ut/rich?)
        (.clearLine rl))
      (.prompt rl false))))

(defn check-readable-cmd [s]
  (list '(fn [s] (when-not (clojure.string/blank? s) (try (read-string s) nil (catch Exception e (.getMessage e))))) s))

(defn check-readable [{:keys [rl state] :as ctx}]
  (call-remote ctx
               (check-readable-cmd (.-line rl))
               (fn [ex-str]
                 (let [warn? (boolean ex-str)]
                   (when (not= warn? (boolean (:warn? @state)))
                     (swap! state assoc :warn? warn?)
                     (set-prompt ctx nil warn?)
                     (.prompt rl true))))))

(defn start [host port]
  (let [istream js/process.stdin
        ostream js/process.stdout
        terminating? (atom false)
        conn-out (.Socket. un/net)
        conn-in (connect conn-out host port true terminating?)]
    (.on conn-in
         "started"
         (fn []
           (let [aux-out (.Socket. un/net)
                 aux-in (connect aux-out host port false terminating?)
                 completer-fn (atom nil)]
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
                                                       (when-let [f @completer-fn]
                                                         (f line cb)))
                                          :next (fn [rl]
                                                  (let [ctx {:istream istream
                                                             :ostream ostream
                                                             :callbacks (atom {})
                                                             :pending-eval (atom nil)
                                                             :conn-in conn-in
                                                             :conn-out conn-out
                                                             :aux-in aux-in
                                                             :aux-out aux-out
                                                             :rl rl
                                                             :state (atom {})}]
                                                    (reset! completer-fn (partial complete ctx))
                                                    (.on conn-in "data" #(did-receive ctx % :conn))
                                                    (.on aux-in "data" #(did-receive ctx % :aux))
                                                    (.on rl "line" (fn [line]
                                                                     (when (ut/rich?)
                                                                       (.clearLine ostream)
                                                                       (.clearScreenDown ostream))
                                                                     (if-let [[_ cmd] (re-matches #"^\s*#__([a-zA-Z0-9]*)?\s*$" line)]
                                                                       (special ctx cmd)
                                                                       (send-command ctx line))))
                                                    (.on rl "close" (fn []
                                                                      (when (ut/rich?)
                                                                        (println))
                                                                      (reset! terminating? true)
                                                                      (ud/dbug :end "conn-out")
                                                                      (.end conn-out)
                                                                      (.end aux-out)))
                                                    (.on rl "SIGINT" #(interrupt ctx))
                                                    (.on istream "error"
                                                         (fn []
                                                           (.exit js/process 143)))
                                                    (.on istream "keypress"
                                                         (fn [chunk key]
                                                           (cond
                                                             (and (.-ctrl key) (= "o" (.-name key)))
                                                             (show-doc ctx true)
                                                             :else
                                                             (do
                                                               (check-readable ctx)
                                                               (show-doc ctx false)))))))}))))))))
