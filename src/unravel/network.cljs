(ns unravel.network
  (:require [clojure.string]
            [cljs.reader :refer [read-string]]
            [unravel.lisp :as ul]
            [unravel.log :as ud])
  (:import [goog.string StringBuffer]))

(def Transform (.-Transform (js/require "stream")))

(defn edn-stream [stream on-read]
  (let [buf (StringBuffer.)
        active? (atom true)]
    (.on stream "readable"
         (fn []
           (when-let [data (.read stream)]
             (when @active?
               (.append buf (.toString data "utf8"))
               (when-let [[v rst] (ul/safe-read-string (.toString buf))]
                 (do
                   (if (and (vector? v) (= :bye (first v)))
                     (do
                       (ud/dbug :done)
                       (reset! active? false))
                     (on-read v)))
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


(defn make-skip
  "Returns a transformer that skips the stream until sentinel. Pipes sentinel
  and all following bytes to writable stream"
  [sentinel]
  (let [buf (StringBuffer.)
        ready? (atom false)
        transform (fn [chunk enc cb]
                    (this-as this
                      (if @ready?
                        (cb nil chunk)
                        (do
                          (.append buf (.toString chunk "utf8"))
                          (let [s (.toString buf)
                                idx (.indexOf s sentinel)]
                            (when (not= -1 idx)
                              (reset! ready? true)
                              (.push this (subs s idx)))
                            (cb))))))]
    (Transform. #js {:transform transform})))

(defn make-edn-stream
  "Returns a transformer that reads EDN forms. Writes CLJS values a readable
  stream"
  []
  (let [buf (StringBuffer.)
        ready? (atom false)
        done? (atom false)
        transform (fn [chunk enc cb]
                    (this-as this
                      (.append buf (.toString chunk "utf8"))
                      (loop []
                        (when-not @done?
                          (when-let [[v rst] (ul/safe-read-string (.toString buf))]
                            (when (and (vector? v) (= :bye (first v)))
                              (reset! done? true))
                            (when-not @ready?
                              (reset! ready? true)
                              (.emit this "started"))
                            (.push this v)
                            (.clear buf)
                            (when rst
                              (do
                                (.append buf rst)
                                (recur))))))
                      (cb)))]
    (Transform. #js {:readableObjectMode true
                     :transform transform})))

(defn make-pr-str-stream
  []
  (let [transform (fn [v enc cb]
                    (this-as this
                      (cb nil (prn-str v))))]
    (Transform. #js {:writableObjectMode true
                     :transform transform})))
