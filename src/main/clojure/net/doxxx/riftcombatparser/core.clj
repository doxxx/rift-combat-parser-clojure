(ns net.doxxx.riftcombatparser.core
  (:use net.doxxx.riftcombatparser.parser)
  (:require [clojure.java.io :as jio]))

(defn test-large-file-performance []
  (with-open [reader (jio/reader "src/test/resources/CombatLog-Large.txt")]
    (let [events (time (doall (parse reader)))
          actors (time (map-actors events))]
      (println (str (count events) " events loaded."))
      (println (str (count actors) " actors found.")))))
