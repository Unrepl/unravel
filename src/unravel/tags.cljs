(ns unravel.tags
  (:require [cljs.reader]))

(defonce ellipsis-counter (atom 0))

(defonce ellipsis-store (atom {}))

(defrecord Ellipsis [get id])

(def unreachable (Ellipsis. nil nil))

(defn ellipsis [m]
  (if (:get m)
    (let [counter (swap! ellipsis-counter inc)]
       (swap! ellipsis-store assoc counter (assoc m :unravel/source :unrepl))
       (Ellipsis. (:get m) counter))
    unreachable))

(defn elide [value]
  (let [counter (swap! ellipsis-counter inc)]
     (swap! ellipsis-store assoc counter {:unravel/source :unravel :value value})
     counter))

(defrecord ClojureVar [name])

(extend-protocol IPrintWithWriter
  Ellipsis
  (-pr-writer [v writer _]
    (if-some [id (:id v)]
      (write-all writer "/" id)
      (write-all writer "/\u29B0" (:id v))))

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
