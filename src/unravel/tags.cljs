(ns unravel.tags
  (:require [cljs.reader]))

(defonce ellipsis-counter (atom 0))

(defonce ellipsis-store (atom {}))

(defrecord Ellipsis [get])

(defn ellipsis [m]
  (map->Ellipsis m))

(extend-protocol IPrintWithWriter
  Ellipsis
  (-pr-writer [v writer _]
    (let [counter (swap! ellipsis-counter inc)]
      (swap! ellipsis-store assoc counter (:get v))
      (write-all writer "#__" counter))))

(defn register-tag-parsers []
  (cljs.reader/register-default-tag-parser! tagged-literal)
  (cljs.reader/register-tag-parser! 'unrepl/... ellipsis))
