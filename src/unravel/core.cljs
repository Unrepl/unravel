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
   :options (spec/* (spec/alt :version #{"--version"}
                              :debug #{"--debug"}
                              :cp (spec/& (spec/cat :_ #{"--classpath" "-c"} :path string?) (spec/conformer #(:path %)))
                              :blob (spec/& (spec/cat :_ #{"--blob"} :path string?) (spec/conformer #(:path %)))))
   :host (spec/? string?) :port (spec/and string? #(re-matches #"\d+" %))))

(defn -main [& more]
  (init)
  (let [{:keys [options host port]}
        (into {} (doto (spec/conform ::cmdline-args more)
                   (some-> #{::spec/invalid}
                           (when (fail "Syntax: unravel [--debug] [-c|--classpath <paths>] [--blob blob1 [--blob blob2 ...]] [<host>] <port>\n        unravel --version")))))
        {:keys [cp version debug blob]}
        (transduce (map (fn [[tag v]] {tag [v]})) (partial merge-with into) {} options)]
    (when version (print-version!))
    (when debug (reset! ul/debug? true))
    (uo/start (or host "localhost")
              port
              {:cp (into [] (comp (mapcat #(str/split % (re-pattern (.-delimiter un/path)))) (distinct)) cp)
               :blobs blob})))
