(ns unravel.core
  (:require [clojure.spec.alpha :as spec]
            [cljs.reader]
            [unravel.log :as ud]
            [unravel.node :as un]
            [unravel.version :as uv]
            [unravel.loop :as uo]
            [unravel.jack-in])
  (:import [goog.string StringBuffer]))

(defn fail [message]
  (println message)
  (js/process.exit 1))

(defn print-version! []
  (println "Unravel" uv/version (str "(Lumo " lumo.core/*lumo-version* ")"))
  (js/process.exit 0))

(def help-text
  "Syntax: unravel [--debug] [-c|--classpath <paths>] [--blob blob1 [--blob blob2 ...]] [--flag flag1 [--flag --flag2 ...]] [<host> <port>]\n        unravel --version")

(defn print-help! []
  (println help-text)
  (js/process.exit 0))

(defn init [])

(defn parse-arg [m [arg nxt :as args]]
  (let [switch (fn [test-fn kw]
                 (when (test-fn arg)
                   [(assoc m kw true) (rest args)]))
        mult (fn mult
               ([test-fn kw] (mult test-fn kw [] identity))
               ([test-fn kw empty-coll] (mult test-fn kw empty-coll identity))
               ([test-fn kw empty-coll val-fn]
                (when (test-fn arg)
                  (assert (some? nxt) "Needs parameter")
                  [(update m kw (fn [elements] (conj (or elements empty-coll) (val-fn nxt)))) (rest (rest args))])))]
    (or
     (switch #{"--version"} :version?)
     (switch #{"--debug"} :debug?)
     (switch #{"--help"} :help?)
     (mult #{"--classpath" "-c"} :cp)
     (mult #{"--blob"} :blobs)
     (mult #{"--flag"} :flags #{} keyword))))

(defn parse-args [args]
  (loop [m {}
         [arg :as args] args]
    (if-let [[m* args*] (parse-arg m args)]
      (recur m* args*)
      (cond
        (nil? arg)
        m

        (.startsWith arg "-")
        (throw (js/Error. (str "Unknown argument: " arg)))

        :else
        (recur (update m :positional (fn [xs] (conj (or xs []) arg)))
               (rest args))))))



(defn jack-in [cb]
  (let [child (.spawn (js/require "child_process")
                      "bash"
                      #js ["-c" unravel.jack-in/payload])
        waiting? (atom true)
        terminate-fn (fn terminate-fn []
                       (.kill child))
        sb (StringBuffer.)]
    (-> child
        (.on "exit" (fn []
                      (-> child .-stdout .destroy)
                      (-> child .-stderr .destroy)))
        (.on "close" (fn [code signal]
                       (ud/dbug :jack-in/closed {:code code :signal signal}))))
    (-> child .-stderr (.on "data" (fn [data]
                                     (.write js/process.stderr data))))
    (-> child .-stdout (.on "data" (fn [data]
                                     (ud/dbug :from-subprocess (.toString data))
                                     (when @waiting?
                                       (.append sb (.toString data))
                                       (if-let [match (re-find #"\[:jack-in/ready.*" (.toString sb))]
                                         (let [[tag {:keys [port]} :as msg] (cljs.reader/read-string match)]
                                           (reset! waiting? false)
                                           (ud/dbug :jack-in/response msg)
                                           (when (not= tag :jack-in/ready)
                                             (throw (js/Error. "Could not parse :jack-in/ready message")))
                                           (cb port terminate-fn)))))))))

(defn -main [& more]
  (init)
  (let [{:keys [version?
                help?
                debug?
                positional] :as args}
        (parse-args more)]
    (when version? (print-version!))
    (when help? (print-help!))
    (when debug? (reset! ud/debug? true))
    (let [jack-in? (case (count positional)
                     2 false
                     0 true
                     (throw (js/Error. "You need to pass 0 or 2 positional arguments")))
          start-fn (fn [host port terminate-fn]
                     (uo/start (or host "localhost")
                               port
                               terminate-fn
                               args))]
      (if jack-in?
        (jack-in (fn [port terminate-fn] (start-fn "localhost" port terminate-fn)))
        (start-fn (first positional) (second positional) (fn []))))))
