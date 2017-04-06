(ns unravel.log)

(def debug? (atom nil))

(defn dbug [& args]
  (when @debug?
    (prn (vec args))))

(defn info [& args]
  (prn (vec args)))
