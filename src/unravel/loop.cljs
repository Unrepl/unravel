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

(defmulti process first)

(defmethod process :prompt [[_ opts] rl]
  (let [ns (:form (get opts 'clojure.core/*ns*))]
    (when ns
      (.setPrompt rl (if (ut/interactive?)
                       (str ns "=> ")
                       "")))
    (.prompt rl true)))

(defmethod process :eval [[_ result counter] rl eval-handlers]
  (let [f (-> @eval-handlers (get counter))]
    (if f
      (f result)
      (ut/cyan #(prn result)))))

(defmethod process :bye [[_ result counter] rl eval-handlers done-cb]
  (done-cb))

(defmethod process :exception [[_ e] rl]
  (ut/red #(println (uu/rstrip-one (with-out-str (ue/print-ex-form (:ex e)))))))

(defmethod process :out [[_ s] rl]
  (.write js/process.stdout s))

(defmethod process :unrepl/hello [command rl])

(defmethod process :started-eval [command rl])
(defmethod process :echo [command rl])

(defmethod process :default [command rl]
  (ud/dbug :unknown-command command))

(defn did-receive [rl command eval-handlers done-cb]
  (ud/dbug :receive command)
  (process command rl eval-handlers done-cb))

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
  (when-let [word (ul/find-word-at line (max 0 (dec cursor)))]
    (println)
    (uw/send! cx eval-counter (str (cmd-doc word)))))

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

(defn special [{:keys [cx eval-counter rl]} cmd]
  (cond
    (or (= "help" cmd))
    (do
      (help)
      (.prompt rl))

    (or (nil? cmd) (re-matches #"^\d*$" cmd))
    (if-let [cmd (get @ug/ellipsis-store (or (some-> cmd js/parseInt) @ug/ellipsis-counter))]
      (uw/send! cx eval-counter (str cmd))
      (.prompt rl))))

(defn send-command [ctx s]
  (uw/send! (:cx ctx) (:eval-counter ctx) s))

(defn start [host port]
  (let [istream js/process.stdin
        ostream js/process.stdout
        eval-handlers (atom {})
        eval-counter (atom 0)
        cx (.Socket. un/net)
        setup-rl (fn [rl]
                   (let [ctx {:istream istream
                              :ostream ostream
                              :eval-handlers eval-handlers
                              :eval-counter eval-counter
                              :cx cx
                              :rl rl}]
                     (uw/edn-stream cx (fn [v done-cb]
                                         (did-receive rl v eval-handlers done-cb)))
                     (.on rl "line" (fn [line]
                                      (if-let [[_ cmd] (re-matches #"^\s*#__([a-zA-Z0-9]*)?\s*$" line)]
                                        (special ctx cmd)
                                        (send-command ctx line))))
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
                              (do-doc cx eval-counter (.-line rl) (.-cursor rl)))))))
        opts #js{:input istream
                 :output ostream
                 :path (un/join-path (un/os-homedir) ".unravel" "history")
                 :maxLength 1000
                 :completer (fn [line cb]
                              (let [word (or (ul/find-word-at line (count line)) "")
                                    timeout (fn []
                                              (println "\n*** completer timed out ***")
                                              (cb nil #js[#js[] word]))
                                    [cb* timeout*] (uu/once-many cb timeout)]
                                (let [counter (uw/send! cx eval-counter (str (cmd-complete word)))]
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
                  (ud/dbug :connect)
                  (.write cx (read-payload))
                  (.write cx "\n")))
      (.on "error" (fn [err]
                     (println "Socket error:" (pr-str err))
                     (js/process.exit 1)))
      (uw/consume-until "[:unrepl/hello"
                        (fn []
                          (when (ut/interactive?)
                            (banner host port))
                          (.createInterface un/readline opts))))))
