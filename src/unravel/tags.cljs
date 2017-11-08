(ns unravel.tags
  (:require [cljs.reader]))

(defonce ellipsis-counter (atom 0))

(defonce ellipsis-store (atom {}))

(defrecord Ellipsis [get])

(defrecord ClojureVar [name])

(extend-protocol IPrintWithWriter
  Ellipsis
  (-pr-writer [v writer _]
    (let [counter (swap! ellipsis-counter inc)]
      (swap! ellipsis-store assoc counter (:get v))
      (write-all writer "#__" counter)))

  ClojureVar
  (-pr-writer [v writer _]
    (write-all writer "#'" (:name v))))

(def tag-map
  {'unrepl/... map->Ellipsis
   'clojure/var ->ClojureVar})

(defn register-tag-parsers []
  (cljs.reader/register-default-tag-parser! tagged-literal)
  (cljs.reader/register-tag-parser! 'unrepl/... map->Ellipsis)
  (cljs.reader/register-tag-parser! 'clojure/var ->ClojureVar))
