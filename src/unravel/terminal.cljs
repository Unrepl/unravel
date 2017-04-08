(ns unravel.terminal)

(defn interactive? [] (.-isTTY js/process.stdin))
(defn rich? [] (.-isTTY js/process.stdout))

(defn tred []
  (when (rich?)
    (.write js/process.stdout "\33[31m")))

(defn tcyan []
  (when (rich?)
    (.write js/process.stdout "\33[36m")))

(defn treset []
  (when (rich?)
    (.write js/process.stdout "\33[0m")))

(defn red [f]
  (tred)
  (f)
  (treset))

(defn cyan [f]
  (tcyan)
  (f)
  (treset))

#_(defn color [f]
    (when (rich?)
      (.write js/process.stdout "\33[35m"))
    (f)
    (treset))
