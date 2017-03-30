(ns socket.repl)

(require '[clojure.core.server :as server]
         '[clojure.main :as main])

(defn repl []
  (main/repl
   :prompt (constantly nil)))

(server/start-server {:name "repl"
                      :port 50505
                      :accept 'socket.repl/repl})

(repl)
