(ns unravel.core)

(def readline (js/require "readline"))
(def rl (.createInterface readline #js{:input js/process.stdin
                                       :output js/process.stdout
                                       :prompt "> "}))

(defn accept [v]
  (println "got" (pr-str v)))

(defn start []
  (.prompt rl)
  (.on rl "line" (fn [line]
                   (accept line)
                   (.prompt rl))))
(defn -main []
  (start))
