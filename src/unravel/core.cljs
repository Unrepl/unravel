(ns unravel.core
  (:require [clojure.spec.alpha :as spec]
            [clojure.string :as str]
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

(defn init [])

(spec/def ::cmdline-args
  (spec/cat
    :options (spec/* (spec/alt
                       :version? #{"--version"}
                       :debug? #{"--debug"}
                       :cp (spec/& (spec/cat :_ #{"--classpath" "-c"} :value string?) (spec/conformer #(:value %)))
                       :blobs (spec/& (spec/cat :_ #{"--blob"} :value string?) (spec/conformer #(:value %)))
                       :flags (spec/& (spec/cat :_ #{"--flag"} :value string?) (spec/conformer #(:value %)))
                       :positionals #(not (.startsWith % "-"))))
    :huh? (spec/* any?)))

(def help-text
  "Syntax: unravel [--debug] [-c|--classpath <paths>] [--blob blob1 [--blob blob2 ...]] [--flag flag1 [--flag --flag2 ...]] [<host> <port>]\n        unravel --version")

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
  (let [options
        (let [{:keys [options huh?]} (spec/conform ::cmdline-args more)]
          (when (seq huh?)
            (fail (str "Do not understand: " (str/join " " huh?) "\n\n" help-text)))
          options)
        {:keys [version? debug? positionals] :as options}
        (reduce (fn [m [k v]]
                  (assoc m k
                    (case k
                      (:debug? :version?) true
                      :cp (into (m k []) (distinct) (str/split v (re-pattern (.-delimiter un/path))))
                      :flags (into (m k #{}) (map (fn [[_ ns name]] (keyword ns name))) (re-seq #"(?:([^\s,/]+)/)?([^\s,]+)" v))
                      (conj (m k []) v)))) 
          {} options)]
    (when version? (print-version!))
    (when debug? (reset! ud/debug? true))
    (if-some [[host port] (case (count positionals)
                            0 nil
                            1 (cons "localhost" positionals)
                            2 positionals)]
      (uo/start host port (fn []) options)
      (jack-in (fn [port terminate-fn] 
                 (uo/start "localhost" port terminate-fn options))))))