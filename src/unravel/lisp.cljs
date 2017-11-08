(ns unravel.lisp
  (:require [clojure.string]
            [cljs.tools.reader :as reader]
            [cljs.tools.reader.reader-types :as reader-types]
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
  (binding [reader/*default-data-reader-fn* tagged-literal]
    (let [r (reader-types/string-push-back-reader s)]
      [(reader/read r)
       (uu/unblank (clojure.string/trim (read-chars r)))])))

(def whitespace-regex #"([\s,])(.*)")
(def word-regex #"([*+=?!_'?a-zA-Z-][*+=?!_'?a-zA-Z0-9/.-]*)(.*)")

(defn tokenize [s]
  (loop [pos 0
         s s
         tokens []]
    (if (clojure.string/blank? s)
      tokens
      (if-let [[_ match rst] (re-matches whitespace-regex s)]
        (recur (+ pos (count match)) rst tokens)
        (if-let [[_ word rst] (re-matches word-regex s)]
          (recur (+ pos (count word))
                 rst
                 (conj tokens {:start pos
                               :end (+ pos (count word))
                               :type :word
                               :value word}))
          (recur (inc pos)
                 (subs s 1) (conj tokens {:start pos
                                          :end (inc pos)
                                          :type :syntax
                                          :value (first s)})))))))

(defn find-word-at [s pos]
  (let [tokens (->> s
                    tokenize
                    (filter (comp #{:word} :type)))]
    (:value (or (some (fn [{:keys [type end] :as token}]
                        (when (< pos end)
                          token))
                      tokens)
                (last tokens)))))
