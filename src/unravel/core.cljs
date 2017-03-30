(ns unravel.core)

(def readline (js/require "readline"))
(def net (js/require "net"))

(defn accept [v]
  (println "typed in:" (pr-str v)))

(defn connect [on-connect on-data]
  (let [cx (.Socket. net)]
    (doto cx
      (.connect 50505 "localhost"
                (fn []
                  (.setNoDelay cx true)
                  (on-connect)))
      (.on "error" (fn [err] (println "Got error:" err)))
      (.on "data" (fn [data]
                    (on-data (.toString data "utf8")))))))

(defn start []
  (let [rl (.createInterface readline #js{:input js/process.stdin
                                          :output js/process.stdout
                                          :prompt ">> "})
        client (connect #(.prompt rl)
                        (fn [data]
                          (println data)))]
    (.on rl "line" (fn [line]
                     (.write client
                             (str line "\n")
                             "utf8")
                     #_(.prompt rl)))))

(defn -main []
  (start))
