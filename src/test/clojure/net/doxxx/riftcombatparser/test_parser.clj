(ns net.doxxx.riftcombatparser.test_parser
  (:use clojure.test)
  (:use net.doxxx.riftcombatparser.parser))

(defn shutdown [f]
  (f)
  (shutdown-agents))

;(use-fixtures :once shutdown)

(deftest parsing-time
  (is (= 36610 (parse-time "10:10:10"))))

(deftest parsing-combat-begin
  (is (= (->CombatToggle 123 true) (parse-line-data 123 " Combat Begin"))))

(deftest parsing-combat-end
  (is (= (->CombatToggle 123 false) (parse-line-data 123 " Combat End"))))

(deftest parsing-combat-event
  (let [data ": ( 7 , T=N#R=O#9223372037794304832 , T=N#R=O#9223372037794304832 , T=X#R=X#0 , T=X#R=X#0 , Arban Chinua , Arban Chinua , 0 , 463220133 , Hellfire Blades ) Arban Chinua's Hellfire Blades fades from Arban Chinua."
        event (->CombatData 1 7 "T=N#R=O#9223372037794304832" "T=N#R=O#9223372037794304832" "T=X#R=X#0" "T=X#R=X#0" "Arban Chinua" "Arban Chinua" 0 463220133 "Hellfire Blades" "Arban Chinua's Hellfire Blades fades from Arban Chinua.")]
    (is (= event (parse-combat-event 1 data)))))

(deftest parsing-file
  (let [events (parse (new java.io.BufferedReader (new java.io.FileReader "C:\\Rift\\RIFT Game\\CombatLog-TEST.txt")))]
    (is (= 295959 (count events)))))

(deftest mapping-actors
  (let [lines ["13:21:43: ( 7 , T=N#R=O#9223372037794304832 , T=N#R=O#9223372037794304832 , T=X#R=X#0 , T=X#R=X#0 , Arban Chinua , Arban Chinua , 0 , 463220133 , Hellfire Blades ) Arban Chinua's Hellfire Blades fades from Arban Chinua."
               "13:21:48: ( 5 , T=P#R=C#227009568756889439 , T=P#R=C#227009568756889439 , T=X#R=X#0 , T=X#R=X#0 , Lucida , Lucida , 0 , 293562978 , Salvation ) Lucida's Salvation heals Lucida for 0. (66 overheal)"
               "13:21:48: ( 8 , T=P#R=C#227009568756889439 , T=N#R=O#9223372037760753784 , T=X#R=X#0 , T=X#R=X#0 , Lucida , Dungeon Practice Dummy , 0 , 1309615220 , Brutalize ) Dungeon Practice Dummy is afflicted by Lucida's Brutalize."
               "13:57:43: ( 7 , T=P#R=O#227009568806991842 , T=P#R=O#227009568806991842 , T=X#R=X#0 , T=X#R=X#0 , Ikani , Ikani , 0 , 755603942 , Armor of Treachery ) Ikani's Armor of Treachery fades from Ikani."]]
    (is (=
          {"T=P#R=O#227009568806991842" "Ikani",
           "T=N#R=O#9223372037760753784" "Dungeon Practice Dummy",
           "T=P#R=C#227009568756889439" "Lucida",
           "T=N#R=O#9223372037794304832" "Arban Chinua"}
          (map-actors (parse-lines lines))))))



