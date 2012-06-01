(ns net.doxxx.riftcombatparser.core
  (:use [net.doxxx.riftcombatparser.parser :as p]))

(let [events (p/parse (new java.io.BufferedReader (new java.io.FileReader "C:\\Rift\\RIFT Game\\CombatLog-TEST.txt")))]
  (time (p/map-actors events)))

(shutdown-agents)
