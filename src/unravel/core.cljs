(ns unravel.core
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as spec]
            [unravel.log :as ul]
            [unravel.version :as uv]
            [unravel.loop :as uo]
            [unravel.node :as un])
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
                       :version #{"--version"}
                       :debug #{"--debug"}
                       :cp (spec/& (spec/cat :_ #{"--classpath" "-c"} :value string?) (spec/conformer #(:value %)))
                       :blobs (spec/& (spec/cat :_ #{"--blob"} :value string?) (spec/conformer #(:value %)))
                       :flags (spec/& (spec/cat :_ #{"--flag"} :value string?) (spec/conformer #(:value %)))
                       :positionals #(not (.startsWith % "-"))))
    :huh? (spec/* any?)))

(def help-text
  "Syntax: unravel [--debug] [-c|--classpath <paths>] [--blob blob1 [--blob blob2 ...]] [--flag flag1 [--flag --flag2 ...]] [<host>] <port>\n        unravel --version")

(defn -main [& more]
  (init)
  (let [options
        (let [{:keys [options huh?]} (spec/conform ::cmdline-args more)]
          (when (seq huh?)
            (fail (str "Do not understand: " (str/join " " huh?) "\n\n" help-text)))
          options)
        {:keys [version debug positionals] :as options}
        (reduce (fn [m [k v]]
                  (assoc m k
                    (case k
                      (:debug :version) true
                      :cp (into (m k []) (distinct) (str/split v (re-pattern (.-delimiter un/path))))
                      :flags (into (m k #{}) (map (fn [[_ ns name]] (keyword ns name))) (re-seq #"(?:([^\s,/]+)/)?([^\s,]+)" v))
                      (conj (m k []) v)))) 
          {} options)
        [host port] (case (count positionals)
                      1 (cons "localhost" positionals)
                      2 positionals)]
    (when version (print-version!))
    (when debug (reset! ul/debug? true))
    (uo/start host port options)))
