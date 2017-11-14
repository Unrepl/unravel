(ns unravel.core
  (:require [clojure.string]
            [clojure.spec.alpha :as spec]
            [unravel.log :as ul]
            [unravel.version :as uv]
            [unravel.loop :as uo])
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
    :options (spec/* (spec/alt :version #{"--version"} :debug #{"--debug"} :cp (spec/& (spec/cat :_ #{"--classpath" "-cp"} :path string?) :path)))
    :host (spec/? string?) :port (spec/and string? #(re-matches #"\d+" %))))

(defn -main [& more]
  (init)
  (let [{:keys [options host port]}
        (into {} (doto (spec/conform ::cmdline-args more)
                   (some-> #{::spec/invalid}
                     (when (fail "Syntax: unravel [--debug] [-cp|--classpath <paths>] [<host>] <port>\n        unravel --version")))))
        {:keys [cp version debug]} (transduce (map (fn [[tag v]] {tag v})) (partial merge-with into) {} options)]
    (when version (print-version!))
    (when debug (reset! ul/debug? true))
    (uo/start (or host "localhost") port)))
