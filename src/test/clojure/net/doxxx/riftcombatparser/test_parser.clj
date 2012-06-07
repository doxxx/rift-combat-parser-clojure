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

(let [events [(->CombatData 10 :direct-damage "T=N#R=O#901" "T=P#R=C#01" "T=X#R=X#0" "T=X#R=X#0" "NPC1" "PC1" 100 1 "Spell1" "text")
              (->CombatData 15 :direct-damage "T=P#R=C#01" "T=N#R=O#901" "T=X#R=X#0" "T=X#R=X#0" "PC1" "NPC1" 300 2 "Spell2" "text")
              (->CombatData 20 :direct-damage "T=N#R=O#901" "T=P#R=C#01" "T=X#R=X#0" "T=X#R=X#0" "NPC1" "PC1" 500 1 "Spell1" "text")
              (->CombatData 0 :direct-damage "T=N#R=O#901" "T=P#R=C#01" "T=X#R=X#0" "T=X#R=X#0" "NPC1" "PC1" 500 1 "Spell1" "text")
              (->CombatData 5 :direct-damage "T=N#R=O#901" "T=P#R=C#01" "T=X#R=X#0" "T=X#R=X#0" "NPC1" "PC1" 500 1 "Spell1" "text")
              (->CombatData 10 :direct-damage "T=N#R=O#901" "T=P#R=C#01" "T=X#R=X#0" "T=X#R=X#0" "NPC1" "PC1" 500 1 "Spell1" "text")
              (->CombatData 1 :direct-damage "T=N#R=O#901" "T=P#R=C#01" "T=X#R=X#0" "T=X#R=X#0" "NPC1" "PC1" 500 1 "Spell1" "text")]
      norm-events (normalize-event-times events)]
  (fact (map :event-time norm-events) => [0 5 10 86390 86395 86400 172791]))

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

(defn simplify-event [event]
  [(:event-time event) (:actor-name event) (:target-name event)])

(let [fight1 [(->CombatData 1 :direct-damage "T=N#R=O#901" "T=P#R=C#01" "T=X#R=X#0" "T=X#R=X#0" "NPC1" "PC1" 100 1 "Spell1" "text")
              (->CombatData 2 :direct-damage "T=P#R=C#01" "T=N#R=O#901" "T=X#R=X#0" "T=X#R=X#0" "PC1" "NPC1" 300 2 "Spell2" "text")
              (->CombatData 3 :direct-damage "T=N#R=O#901" "T=P#R=C#01" "T=X#R=X#0" "T=X#R=X#0" "NPC1" "PC1" 500 1 "Spell1" "text")]
      fight2 [(->CombatData 8 :direct-damage "T=N#R=O#901" "T=P#R=C#01" "T=X#R=X#0" "T=X#R=X#0" "NPC1" "PC1" 100 1 "Spell1" "text")
              (->CombatData 9 :direct-damage "T=P#R=C#01" "T=N#R=O#901" "T=X#R=X#0" "T=X#R=X#0" "PC1" "NPC1" 300 2 "Spell2" "text")
              (->CombatData 10 :direct-damage "T=N#R=O#901" "T=P#R=C#01" "T=X#R=X#0" "T=X#R=X#0" "NPC1" "PC1" 500 1 "Spell1" "text")]
      events (concat fight1 fight2)
      fights (split-fights events)]
  (fact (map (fn [x] (map simplify-event x)) fights) => [(map simplify-event fight1) (map simplify-event fight2)]))

;[[#net.doxxx.riftcombatparser.parser.CombatData {:event-time 1, :event-type :direct-damage, :actor-id "T=N#R=O#901", :target-id "T=P#R=C#01", :actor-owner-id "T=X#R=X#0", :target-owner-id "T=X#R=X#0", :actor-name "NPC1", :target-name "PC1", :amount 100, :spell-id 1, :spell-name "Spell1", :text "text"}
;  #net.doxxx.riftcombatparser.parser.CombatData {:event-time 2, :event-type :direct-damage, :actor-id "T=P#R=C#01", :target-id "T=N#R=O#901", :actor-owner-id "T=X#R=X#0", :target-owner-id "T=X#R=X#0", :actor-name "PC1", :target-name "NPC1", :amount 300, :spell-id 2, :spell-name "Spell2", :text "text"}
;  #net.doxxx.riftcombatparser.parser.CombatData {:event-time 3, :event-type :direct-damage, :actor-id "T=N#R=O#901", :target-id "T=P#R=C#01", :actor-owner-id "T=X#R=X#0", :target-owner-id "T=X#R=X#0", :actor-name "NPC1", :target-name "PC1", :amount 500, :spell-id 1, :spell-name "Spell1", :text "text"}]
; [#net.doxxx.riftcombatparser.parser.CombatData {:event-time 9, :event-type :direct-damage, :actor-id "T=P#R=C#01", :target-id "T=N#R=O#901", :actor-owner-id "T=X#R=X#0", :target-owner-id "T=X#R=X#0", :actor-name "PC1", :target-name "NPC1", :amount 300, :spell-id 2, :spell-name "Spell2", :text "text"}
;  #net.doxxx.riftcombatparser.parser.CombatData {:event-time 10, :event-type :direct-damage, :actor-id "T=N#R=O#901", :target-id "T=P#R=C#01", :actor-owner-id "T=X#R=X#0", :target-owner-id "T=X#R=X#0", :actor-name "NPC1", :target-name "PC1", :amount 500, :spell-id 1, :spell-name "Spell1", :text "text"}]]

(let [lines ["23:54:54: ( 11 , T=P#R=O#231090956094727465 , T=N#R=O#9223372048838202481 , T=X#R=X#0 , T=X#R=X#0 , Rofldotz , Abyssal Infiltrator , 0 , 0 ,  ) Rofldotz has slain Abyssal Infiltrator!"
             "23:54:54: ( 9 , T=P#R=O#231090956094727465 , T=N#R=O#9223372048838202481 , T=X#R=X#0 , T=X#R=X#0 , Rofldotz , Abyssal Infiltrator , 0 , 1571501358 , Countdown ) Rofldotz's Countdown dissipates from Abyssal Infiltrator."
             "23:54:54: ( 3 , T=P#R=O#231090956094727465 , T=N#R=O#9223372048838202481 , T=X#R=X#0 , T=X#R=X#0 , Rofldotz , Abyssal Infiltrator , 1387 , 795424656 , Cinder Burst ) Rofldotz's Cinder Burst hits Abyssal Infiltrator for 1387 Fire damage. (1017 overkill)"
             "23:54:58 Combat Begin"
             "23:54:58: ( 3 , T=P#R=C#231090956085911584 , T=N#R=O#9223372045142730245 , T=X#R=X#0 , T=X#R=X#0 , Lucida , Boss Practice Dummy , 69 , 548293169 , Auto Attack ) Lucida's Auto Attack hits Boss Practice Dummy for 69 Physical damage."
             "23:55:00: ( 6 , T=P#R=G#231090956086045364 , T=N#R=G#9223372049233347913 , T=X#R=X#0 , T=P#R=G#231090956086045364 , Thelastresort , Bane , 0 , 156052578 , Combined Effort ) Bane gains Thelastresort's Combined Effort."
             "23:55:00: ( 6 , T=P#R=G#231090956086045364 , T=P#R=G#231090956086045364 , T=X#R=X#0 , T=X#R=X#0 , Thelastresort , Thelastresort , 0 , 173580775 , Fae Hammer ) Thelastresort gains Thelastresort's Fae Hammer."
             "23:55:00: ( 27 , T=P#R=G#231090956086045364 , T=P#R=G#231090956086045364 , T=X#R=X#0 , T=X#R=X#0 , Thelastresort , Thelastresort , 44 , 173580775 , Fae Hammer ) Thelastresort's Fae Hammer gives Thelastresort 44 Mana."]
     events (normalize-event-times (parse-lines lines))
     mod-events (insert-death-later (first events) (rest events))]
  (fact mod-events => (concat (take 2 (drop 1 events)) [(->DeathEvent 0 :death "T=N#R=O#9223372048838202481" (first events))] (drop 3 events))))
