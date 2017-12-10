(ns unravel.lisp
  (:require [clojure.string]
            [cljs.tools.reader :as reader]
            [cljs.tools.reader.reader-types :as reader-types]
            [unravel.tags :as ut]
            [unravel.util :as uu])
  (:import [goog.string StringBuffer]))

(defn- read-chars
  [reader]
  (let [sb (StringBuffer.)]
    (loop [c (reader-types/read-char reader)]
      (if-not (nil? c)
        (do
          (.append sb c)
          (recur (reader-types/read-char reader)))
        (str sb)))))

(defn safe-read-string [s]
  (binding [reader/*default-data-reader-fn* tagged-literal
            reader/*data-readers* ut/tag-map]
    (let [r (reader-types/string-push-back-reader s)]
      [(reader/read r)
       (uu/unblank (clojure.string/trim (read-chars r)))])))

(def lead-word-regex #"^[<>*+=?!_?a-zA-Z-][<>*+=?!_'?a-zA-Z0-9/.-]*")
(def word-prefix-regex #"[<>*+=?!_?a-zA-Z-][<>*+=?!_'?a-zA-Z0-9/.-]*$")
(def word-suffix-regex #"^[<>*+=?!_'?a-zA-Z0-9/.-]*")

(defn find-word-at [s pos]
  (if-some [pre (re-find word-prefix-regex (subs s 0 pos))]
    (str pre (re-find word-suffix-regex (subs s pos)))
    (re-find lead-word-regex (subs s pos))))
