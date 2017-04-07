(ns unravel.network
  (:require [clojure.string]
            [cljs.reader :refer [read-string]]
            [unravel.lisp :as ul]
            [unravel.log :as ud])
  (:import [goog.string StringBuffer]))

(def Transform (.-Transform (js/require "stream")))

(defn send! [conn eval-counter s]
  (ud/dbug :send s)
  (.write conn s "utf8")
  (.write conn "\n" "utf8")
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
