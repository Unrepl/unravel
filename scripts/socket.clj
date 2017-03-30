(ns socket.repl)

(require '[clojure.core.server :as server]
         '[clojure.main :as main])

(load-file "scripts/payload.clj")

(defn repl []
  #_(main/repl
     :prompt (constantly nil))
  (unrepl.repl/start))

(server/start-server {:name "repl"
                      :port 50505
                      :accept 'socket.repl/repl})

(repl)
