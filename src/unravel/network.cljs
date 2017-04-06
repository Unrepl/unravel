(ns unravel.network
  (:require [clojure.string]
            [cljs.reader :refer [read-string]]
            [unravel.lisp :as ul]
            [unravel.log :as ud])
  (:import [goog.string StringBuffer]))

(defn edn-stream [stream on-read]
  (let [buf (StringBuffer.)
        active? (atom true)
        done-cb (fn []
                  (ud/dbug :done)
                  (reset! active? false))]
    (.on stream "readable"
         (fn []
           (when-let [data (.read stream)]
             (when @active?
               (.append buf (.toString data "utf8"))
               (when-let [[v rst] (ul/safe-read-string (.toString buf))]
                 (on-read v done-cb)
                 (.clear buf)
                 (when rst
                   (.unshift stream (js/Buffer.from rst "utf8"))))))))))

(defn consume-until [stream sentinel cb]
  (let [buf (StringBuffer.)
        !on-readable (atom nil)
        on-readable (fn []
                      (when-let [data (.read stream)]
                        (.append buf (.toString data "utf8"))
                        (let [s (.toString buf)
                              idx (.indexOf s sentinel)]
                          (when (not= -1 idx)
                            (.removeListener stream "readable" @!on-readable)
                            (cb)
                            (.unshift stream (js/Buffer.from (subs s idx) "utf8"))))))]
    (reset! !on-readable on-readable)
    (.on stream "readable" on-readable)))

(defn send! [cx eval-counter s]
  (ud/dbug :send s)
  (.write cx s "utf8")
  (.write cx "\n" "utf8")
  (swap! eval-counter inc))
