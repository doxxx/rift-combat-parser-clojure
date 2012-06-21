(ns net.doxxx.riftcombatparser.util
  (:require [clojure.pprint :as pp])
  (:import java.io.StringWriter))

(def debug
  (= (System/getProperty "debug") "true"))

(defn debug-log [msg & args]
  (when debug (apply printf msg args) (newline)))

(defmulti pprint-str class)

(defmethod pprint-str :default [x]
  (.toString x))

;(defn pprint-str [x]
;  (let [w (StringWriter.)]
;    (pp/pprint x w)
;    (.toString w)))