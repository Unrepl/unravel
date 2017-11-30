(ns unravel.pprint
  (:require [net.cgrand.packed-printer.core :as core]
    [net.cgrand.packed-printer.text.edn :as te]
    [unravel.tags :as tags]))

(defn ansi [text ansi-text]
  {:length (count text)
   :text ansi-text
   :start-length (count text)
   :start-text ansi-text
   :br-after? true})

(defn nobr [text]
  {:length (count text)
   :text text
   :start-length (count text)
   :start-text text})

(defn spans
  "Turns x into a collection of spans for layout. Options supported are:
 * kv-indent the amount of spaces by which to indent a value when it appears
   at the start of a line (default 2),
 * coll-indents a map of collection start delimiters (as strings) to the amount
   by which to indent (default: length of the delimiter)."
  [x {:keys [kv-indent coll-indents] :or {kv-indent 2 coll-indents {}}}]
  (let [delims (into te/delims (map (fn [[s i]] [s (te/opening s i)])) coll-indents)
        kv-open
        {:length 0
         :text ""
         :start-length 0
         :start-text ""
         :indent kv-indent}]
    (letfn [(coll-spans
              ([x] (coll-spans x [te/space] spans))
              ([x sp spans]
                (sequence (comp (map spans) (interpose sp) cat) x)))
            (kv-spans [[k v]]
              (if (instance? unravel.tags/Ellipsis k)
                (spans v)
                (-> [kv-open] (into (spans k)) (conj te/space) (into (spans v)) (conj te/kv-close))))
            (spans [x]
              (cond
                (tagged-literal? x)
                (case (:tag x)
                  'unrepl/string (let [[s e] (:form x)
                                       s (pr-str s)
                                       s (subs s 0 (dec (count s)))] (cons (nobr s) (spans e)))
                  (concat [kv-open (str "#" (pr-str (:tag x))) te/space] (spans (:form x)) [te/kv-close]))
                (vector? x) (concat [(delims "[")] (coll-spans x) [(delims "]")])
                (set? x) (concat [(delims "#{")]
                           (coll-spans (if-some [e (some #(when (instance? unravel.tags/Ellipsis %) %) x)]
                                         (concat (disj x e) [e])
                                         x))
                           [(delims "}")])
                (seq? x) (concat [(delims "(")] (coll-spans x) [(delims ")")])
                (instance? unravel.tags/Ellipsis x) [(let [s (if-some [id (:id x)] (str "#__" id) "#_\u29B0")]
                                                       (ansi s (str "\33[4m" s "\33[24m")))]
                (map? x) (if-some [kv (find x tags/unreachable)]
                           (concat [(delims "{")] (coll-spans (concat (dissoc x tags/unreachable) [kv]) [te/comma te/space] kv-spans) [(delims "}")])
                           (concat [(delims "{")] (coll-spans x [te/comma te/space] kv-spans) [(delims "}")]))
                :else [(pr-str x)]))]
    (spans x))))

(defmethod core/spans [:text :unrepl/edn] [x to-as opts]
  (spans x opts))

