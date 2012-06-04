(ns net.doxxx.riftcombatparser.parser
  (:require clojure.set))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Event Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def event-types #{:begin-casting
                   :interrupted
                   :direct-damage
                   :damage-over-time
                   :heal
                   :buff-gain
                   :buff-fade
                   :debuff-gain
                   :debuff-fade
                   :miss
                   :slain
                   :died
                   :env-damage
                   :dodge
                   :parry
                   :resist
                   :crit-damage
                   :favor-gain
                   :immune
                   :power-gain
                   :crit-heal})

(defn event-type? [x] (contains? event-types x))

(def event-type-to-int
  (zipmap
    [:begin-casting
     :interrupted
     :direct-damage
     :damage-over-time
     :heal
     :buff-gain
     :buff-fade
     :debuff-gain
     :debuff-fade
     :miss
     :slain
     :died
     :env-damage
     :dodge
     :parry
     :resist
     :crit-damage
     :favor-gain
     :immune
     :power-gain
     :crit-heal]
    [1 2 3 4 5 6 7 8 9 10 11 12 14 15 16 19 23 24 26 27 28]))

(def int-to-event-type (clojure.set/map-invert event-type-to-int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Parsing Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; groups: hr min sec
(def time-re #"([0-9][0-9]):([0-9][0-9]):([0-9][0-9])")
; groups: time, data
(def line-re #"^([0-9][0-9]:[0-9][0-9]:[0-9][0-9])(.+)$")
; groups: state
(def combat-toggle-re #" Combat (Begin|End)")
; groups: data text
(def combat-event-re #": \( (.+?) \) (.+)")
; groups: event-type actor-id target-id actor-owner-id target-owner-id actor-name target-name amount spell-id spell-name
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Event Processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- update-actors [actors event]
  (assoc actors (:actor-id event) (:actor-name event) (:target-id event) (:target-name event)))

(defn map-actors [events]
  (loop [actors {}
         events events]
    (if (empty? events)
      actors
      (recur (update-actors actors (first events)) (rest events)))))
