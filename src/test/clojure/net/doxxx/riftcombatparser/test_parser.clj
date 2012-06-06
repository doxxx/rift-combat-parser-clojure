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

(let [lines ["13:21:43: ( 7 , T=N#R=O#9223372037794304832 , T=N#R=O#9223372037794304832 , T=X#R=X#0 , T=X#R=X#0 , Arban Chinua , Arban Chinua , 0 , 463220133 , Hellfire Blades ) Arban Chinua's Hellfire Blades fades from Arban Chinua."
             "13:21:48: ( 5 , T=P#R=C#227009568756889439 , T=P#R=C#227009568756889439 , T=X#R=X#0 , T=X#R=X#0 , Lucida , Lucida , 0 , 293562978 , Salvation ) Lucida's Salvation heals Lucida for 0. (66 overheal)"
             "13:21:48: ( 8 , T=P#R=C#227009568756889439 , T=N#R=O#9223372037760753784 , T=X#R=X#0 , T=X#R=X#0 , Lucida , Dungeon Practice Dummy , 0 , 1309615220 , Brutalize ) Dungeon Practice Dummy is afflicted by Lucida's Brutalize."
             "13:57:43: ( 7 , T=P#R=O#227009568806991842 , T=P#R=O#227009568806991842 , T=X#R=X#0 , T=X#R=X#0 , Ikani , Ikani , 0 , 755603942 , Armor of Treachery ) Ikani's Armor of Treachery fades from Ikani."]]
  (fact (map-entities (parse-lines lines)) => {"T=P#R=O#227009568806991842" "Ikani",
                                               "T=N#R=O#9223372037760753784" "Dungeon Practice Dummy",
                                               "T=P#R=C#227009568756889439" "Lucida",
                                               "T=N#R=O#9223372037794304832" "Arban Chinua"}))

(with-open [reader (jio/reader "src/test/resources/CombatLog.txt")]
  (let [events (parse reader)]
    (fact (count events) => 100)))

(fact (unpack-entity-id "T=N#R=O#9223372037794304832") => ["N", "O", "9223372037794304832"])

(fact (npc? "T=N#R=O#9223372037794304832") => true)
(fact (pc? "T=P#R=C#227009568756889439") => true)
(fact (nobody? "T=X#R=X#0") => true)

(let [events [(->CombatData 1 :direct-damage "T=N#R=O#901" "T=P#R=C#01" "T=X#R=X#0" "T=X#R=X#0" "NPC1" "PC1" 100 1 "Spell1" "text")
              (->CombatData 2 :direct-damage "T=P#R=C#01" "T=N#R=O#901" "T=X#R=X#0" "T=X#R=X#0" "PC1" "NPC1" 300 2 "Spell2" "text")
              (->CombatData 3 :direct-damage "T=N#R=O#901" "T=P#R=C#01" "T=X#R=X#0" "T=X#R=X#0" "NPC1" "PC1" 500 1 "Spell1" "text")]
      fights (split-fights events)]
  (fact (count fights) => 1)
  (fact (first fights) => events))
