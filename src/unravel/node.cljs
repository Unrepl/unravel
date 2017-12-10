(ns unravel.node)

(def fs (js/require "fs"))
(def path (js/require "path"))
(def join-path (.-join path))
(def readline (js/require "historic-readline"))
(def net (js/require "net"))
(def os-homedir (js/require "os-homedir"))
(def open-jar (.-Open (js/require "unzipper")))

(defn locate [& segments]
  (apply join-path (into [(or js/process.env.UNRAVEL_HOME ".")] segments)))
