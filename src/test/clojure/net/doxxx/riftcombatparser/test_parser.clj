(ns net.doxxx.riftcombatparser.test_parser
  (:use net.doxxx.riftcombatparser.parser)
  (:use midje.sweet)
  (:require [clojure.java.io :as jio]))

(fact (calc-time 10 10 10) => 36610)

(fact (parse-line "10:10:10 Combat Begin") => (->CombatToggle 36610 true))

(fact (parse-line "10:10:10 Combat End") => (->CombatToggle 36610 false))

(fact (parse-line "10:10:10: ( 7 , T=N#R=O#9223372037794304832 , T=N#R=O#9223372037794304832 , T=X#R=X#0 , T=X#R=X#0 , Arban Chinua , Arban Chinua , 0 , 463220133 , Hellfire Blades ) Arban Chinua's Hellfire Blades fades from Arban Chinua.")
  => (->CombatData 36610 :buff-fade "T=N#R=O#9223372037794304832" "T=N#R=O#9223372037794304832" "T=X#R=X#0" "T=X#R=X#0" "Arban Chinua" "Arban Chinua" 0 463220133 "Hellfire Blades" "Arban Chinua's Hellfire Blades fades from Arban Chinua."))

(fact (unpack-entity-id "T=N#R=O#9223372037794304832") => ["N", "O", "9223372037794304832"])

(fact (npc? "T=N#R=O#9223372037794304832") => true)
(fact (pc? "T=P#R=C#227009568756889439") => true)
(fact (nobody? "T=X#R=X#0") => true)
