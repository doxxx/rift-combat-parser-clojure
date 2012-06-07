(ns net.doxxx.riftcombatparser.core
  (:use net.doxxx.riftcombatparser.parser)
  (:require [clojure.java.io :as jio]))

(defn test-large-file-performance []
  (with-open [reader (jio/reader "src/test/resources/CombatLog-Large.txt")]
    (let [events (time (doall (parse reader)))
          entities (time (map-entities events))]
      (println (str (count events) " events loaded."))
      (println (str (count entities) " entities found.")))))

(defn test-large-file-split []
  (with-open [reader (jio/reader "src/test/resources/CombatLog-Large.txt")]
    (let [events (time (doall (normalize-event-times (parse reader))))
          entities (time (map-entities events))
          fights (time (split-fights events))]
      (println (str (count events) " events loaded."))
      (println (str (count entities) " entities found."))
      (println (str (count fights) " fights found."))
      (println (map (fn [f] (fight-duration f)) fights))
      fights)))
