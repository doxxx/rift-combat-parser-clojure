(ns net.doxxx.riftcombatparser.core
  (:use net.doxxx.riftcombatparser.parser))

(defn test-large-file-performance []
  (let [filename "src/test/resources/CombatLog-Large.txt"
        events (parse (new java.io.BufferedReader (new java.io.FileReader filename)))
        actors (time (map-actors events))]
    (println (str (count events) " events loaded."))
    (println (str (count actors) " actors found."))))
