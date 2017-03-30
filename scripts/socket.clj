(ns socket.repl)

(require '[clojure.core.server :as server]
         '[clojure.main :as main])

#_(load-file "scripts/payload.clj")

(defn repl []
  (main/repl)
  #_(main/repl
     :prompt (constantly nil))
  #_(unrepl.repl/start))

(server/start-server {:name "repl"
                      :port 50505
                      :accept 'socket.repl/repl})

(repl)
