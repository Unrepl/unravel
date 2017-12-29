(ns unravel.terminal)

(defn interactive? [] (.-isTTY js/process.stdin))
(defn rich? [] (.-isTTY js/process.stdout))

(defn- with-sgr [code]
  (let [code (str "\33[" code "m")]
    (if (rich?)
      (fn [f]
        (try
          (.write js/process.stdout code)
          (f)
          (finally
            (.write js/process.stdout "\33[0m"))))
      #(%))))

(def red (with-sgr "31"))
(def cyan (with-sgr "36"))
(def yellow (with-sgr "33"))

#_(defn color [f]
    (when (rich?)
      (.write js/process.stdout "\33[35m"))
    (f)
    (treset))
