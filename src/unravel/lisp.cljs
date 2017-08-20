(ns unravel.lisp
  (:require [clojure.string]
            [unravel.util :as uu]
            [cljs.tools.reader.reader-types :as reader-types]
            [cljs.tools.reader :as reader])
  (:import [goog.string StringBuffer]))

(defn- reader-eof? [msg]
  (or
   (= "EOF while reading" msg)
   (= "EOF while reading string" msg)))

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
  (let [reader (reader-types/string-push-back-reader s)
        r (try
            (reader/read reader true ::eof)
            (catch js/Error e ::eof))]
    (when (not= ::eof r)
      [r (uu/unblank (clojure.string/trim (read-chars reader)))])))

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
