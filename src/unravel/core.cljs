(ns unravel.core)

(def readline (js/require "readline"))
(def net (js/require "net"))

(defn accept [v]
  (println "result:" (pr-str v)))

(defn start []
  (let [rl (.createInterface readline #js{:input js/process.stdin
                                          :output js/process.stdout
                                          :prompt ">> "})]
    (.prompt rl)
    (.on rl "line" (fn [line]
                     (accept line)
                     (.prompt rl)))))
(defn -main []
  (start))
