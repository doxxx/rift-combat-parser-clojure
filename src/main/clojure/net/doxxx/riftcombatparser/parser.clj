(ns net.doxxx.riftcombatparser.parser)

; hh, mm, ss
(def time-re #"([0-9][0-9]):([0-9][0-9]):([0-9][0-9])")
; time, data
(def line-re #"^([0-9][0-9]:[0-9][0-9]:[0-9][0-9])(.+)$")
; state
(def combat-toggle-re #" Combat (Begin|End)")
; data, text
(def combat-event-re #": \( (.+?) \) (.+)")
; event-type actor-id target-id actor-owner-id target-owner-id actor-name target-name amount spell-id spell-name
(def combat-data-re #"([0-9]+) , (T=.+) , (T=.+) , (T=.+) , (T=.+) , (.*?) , (.*?) , (-?[0-9]*) , ([0-9]*) , (.*?)")

(defrecord CombatToggle [event-time in-combat])
(defrecord CombatData [event-time event-type actor-id target-id actor-owner-id target-owner-id actor-name target-name amount spell-id spell-name text])

(defn parse-time [s]
  (let [[_ hr min sec] (re-matches time-re s)]
    (+ (* (Integer/parseInt hr) 60 60) (* (Integer/parseInt min) 60) (Integer/parseInt sec))))

(defn parse-combat-data [event-time data text]
  (let [[_ event-type actor-id target-id actor-owner-id target-owner-id actor-name target-name amount spell-id spell-name] (re-matches combat-data-re data)]
    (CombatData. event-time (Integer/parseInt event-type) actor-id target-id actor-owner-id target-owner-id actor-name target-name (Integer/parseInt amount) (Integer/parseInt spell-id) spell-name text)))

(defn parse-combat-event [event-time data]
  (let [[_ combat-data text] (re-matches combat-event-re data)]
    (if (nil? combat-data)
      (println (str "Cannot parse combat event: " data))
      (parse-combat-data event-time combat-data text))))

(defn parse-line-data [event-time data]
  (let [[_ state] (re-matches combat-toggle-re data)]
    (if (nil? state)
      (parse-combat-event event-time data) ; (parse-combat-data event-time combat-data)
      (CombatToggle. event-time (= state "Begin")))))

(defn parse-line [line]
  (let [[_ event-time data] (re-matches line-re line)]
    (parse-line-data (parse-time event-time) data)))

(defn parse-lines [lines]
  (pmap parse-line lines))

(defn parse [reader]
  (parse-lines (line-seq reader)))

(defn update-actors [actors event]
  (assoc actors (:actor-id event) (:actor-name event) (:target-id event) (:target-name event)))

(defn map-actors [events]
  (loop [actors {} events events]
    (if (empty? events)
      actors
      (recur (update-actors actors (first events)) (rest events)))))

; tests
(assert (= 36610 (parse-time "10:10:10")))
(assert (= (CombatToggle. 1 true) (parse-line-data 1 " Combat Begin")))
(assert (= (CombatToggle. 1 false) (parse-line-data 1 " Combat End")))
(def data ": ( 7 , T=N#R=O#9223372037794304832 , T=N#R=O#9223372037794304832 , T=X#R=X#0 , T=X#R=X#0 , Arban Chinua , Arban Chinua , 0 , 463220133 , Hellfire Blades ) Arban Chinua's Hellfire Blades fades from Arban Chinua.")
(def event (CombatData. 1 7 "T=N#R=O#9223372037794304832" "T=N#R=O#9223372037794304832" "T=X#R=X#0" "T=X#R=X#0" "Arban Chinua" "Arban Chinua" 0 463220133 "Hellfire Blades" "Arban Chinua's Hellfire Blades fades from Arban Chinua."))
(assert (= event (parse-line-data 1 data)))
(def lines ["13:21:43: ( 7 , T=N#R=O#9223372037794304832 , T=N#R=O#9223372037794304832 , T=X#R=X#0 , T=X#R=X#0 , Arban Chinua , Arban Chinua , 0 , 463220133 , Hellfire Blades ) Arban Chinua's Hellfire Blades fades from Arban Chinua."
            "13:21:48: ( 5 , T=P#R=C#227009568756889439 , T=P#R=C#227009568756889439 , T=X#R=X#0 , T=X#R=X#0 , Lucida , Lucida , 0 , 293562978 , Salvation ) Lucida's Salvation heals Lucida for 0. (66 overheal)"
            "13:21:48: ( 8 , T=P#R=C#227009568756889439 , T=N#R=O#9223372037760753784 , T=X#R=X#0 , T=X#R=X#0 , Lucida , Dungeon Practice Dummy , 0 , 1309615220 , Brutalize ) Dungeon Practice Dummy is afflicted by Lucida's Brutalize."
            "13:57:43: ( 7 , T=P#R=O#227009568806991842 , T=P#R=O#227009568806991842 , T=X#R=X#0 , T=X#R=X#0 , Ikani , Ikani , 0 , 755603942 , Armor of Treachery ) Ikani's Armor of Treachery fades from Ikani."])
(assert (=
          {"T=P#R=O#227009568806991842" "Ikani",
           "T=N#R=O#9223372037760753784" "Dungeon Practice Dummy",
           "T=P#R=C#227009568756889439" "Lucida",
           "T=N#R=O#9223372037794304832" "Arban Chinua"}
          (map-actors (parse-lines lines))))

;(let [events (parse (new java.io.BufferedReader (new java.io.FileReader "C:\\Rift\\RIFT Game\\CombatLog-TEST.txt")))]
;  (time (map-actors events)))
;
;(let [events (parse (new java.io.BufferedReader (new java.io.FileReader "C:\\Rift\\RIFT Game\\CombatLog-TEST.txt")))]
;  (time (map-actors events)))
;
;(let [events (parse (new java.io.BufferedReader (new java.io.FileReader "C:\\Rift\\RIFT Game\\CombatLog-TEST.txt")))]
;  (time (map-actors events)))
