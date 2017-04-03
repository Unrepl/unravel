(ns socket.repl)

(require '[clojure.core.server :as server]
         '[clojure.main :as main])

(load-file "scripts/payload.clj")

(defn repl []
  (main/repl)
  #_(main/repl
     :prompt (constantly nil))
  #_(unrepl.repl/start))

#_(server/start-server {:name "repl"
                        :port 50505
                        :accept 'socket.repl/repl})

#_(repl)
(unrepl.repl/start)
