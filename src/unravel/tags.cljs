(ns unravel.tags
  (:require [cljs.reader]))

(defonce ellipsis-counter (atom 0))

(defonce ellipsis-store (atom {}))

(defrecord Ellipsis [get id])

(def unreachable (Ellipsis. nil nil))

(defn ellipsis [{:keys [get]}]
  (if get
    (let [counter (swap! ellipsis-counter inc)]
       (swap! ellipsis-store assoc counter get)
       (Ellipsis. get counter))
    unreachable))

(defrecord ClojureVar [name])

(extend-protocol IPrintWithWriter
  Ellipsis
  (-pr-writer [v writer _]
    (if-some [id (:id v)]
      (write-all writer "#__" id)
      (write-all writer "#__\u29B0" (:id v))))

  ClojureVar
  (-pr-writer [v writer _]
    (write-all writer "#'" (:name v))))

(def tag-map
  {'unrepl/... ellipsis
   'clojure/var ->ClojureVar})

(defn register-tag-parsers []
  (cljs.reader/register-default-tag-parser! tagged-literal)
  (doseq [[tag parser] tag-map]
    (cljs.reader/register-tag-parser! tag parser)))
