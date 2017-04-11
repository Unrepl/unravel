(ns unravel.log)

(def debug? (atom nil))

(defn err-prn
  "Print directly to stderr, circumventing *out*"
  [& args]
  ;; TODO: flush?
  (.write js/process.stderr (apply prn-str args)))

(defn dbug [& args]
  (when @debug?
    (err-prn (vec args))))

(defn info [& args]
  (err-prn (vec args)))
