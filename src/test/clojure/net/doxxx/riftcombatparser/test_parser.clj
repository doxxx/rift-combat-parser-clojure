(ns net.doxxx.riftcombatparser.test_parser
  (:use net.doxxx.riftcombatparser.parser)
  (:use midje.sweet)
  (:require [clojure.java.io :as jio]))

(fact (parse-time "10:10:10") => 36610)

(fact (parse-line-data 123 " Combat Begin") => (->CombatToggle 123 true))

(fact (parse-line-data 123 " Combat End") => (->CombatToggle 123 false))

(let [data ": ( 7 , T=N#R=O#9223372037794304832 , T=N#R=O#9223372037794304832 , T=X#R=X#0 , T=X#R=X#0 , Arban Chinua , Arban Chinua , 0 , 463220133 , Hellfire Blades ) Arban Chinua's Hellfire Blades fades from Arban Chinua."
      event (->CombatData 1 :buff-fade "T=N#R=O#9223372037794304832" "T=N#R=O#9223372037794304832" "T=X#R=X#0" "T=X#R=X#0" "Arban Chinua" "Arban Chinua" 0 463220133 "Hellfire Blades" "Arban Chinua's Hellfire Blades fades from Arban Chinua.")]
  (fact (parse-combat-event 1 data) => event))

(with-open [reader (jio/reader "src/test/resources/CombatLog.txt")]
  (let [events (parse reader)]
    (fact (count events) => 100)))

(fact (unpack-entity-id "T=N#R=O#9223372037794304832") => ["N", "O", "9223372037794304832"])

(fact (npc? "T=N#R=O#9223372037794304832") => true)
(fact (pc? "T=P#R=C#227009568756889439") => true)
(fact (nobody? "T=X#R=X#0") => true)
