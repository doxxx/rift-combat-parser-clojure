(ns net.doxxx.riftcombatparser.parser
  (:require clojure.set))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Event Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def event-types {:begin-casting 1
                  :interrupted 2
                  :direct-damage 3
                  :damage-over-time 4
                  :heal 5
                  :buff-gain 6
                  :buff-fade 7
                  :debuff-gain 8
                  :debuff-fade 9
                  :miss 10
                  :slain 11
                  :died 12
                  :env-damage 14
                  :dodge 15
                  :parry 16
                  :resist 19
                  :crit-damage 23
                  :favor-gain 24
                  :immune 26
                  :power-gain 27
                  :crit-heal 28})

(defn event-type? [x] (contains? event-types x))

(def int-to-event-type (clojure.set/map-invert event-types))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Parsing Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; groups: hr min sec
(def time-re #"([0-9][0-9]):([0-9][0-9]):([0-9][0-9])")
; groups: time, data
(def line-re #"^([0-9][0-9]:[0-9][0-9]:[0-9][0-9])(.+)$")
; groups: state
(def combat-toggle-re #" Combat (Begin|End)")
; groups: event-type actor-id target-id actor-owner-id target-owner-id actor-name target-name amount spell-id spell-name text
(def combat-event-re #": \( ([0-9]+) , (T=.+) , (T=.+) , (T=.+) , (T=.+) , (.*?) , (.*?) , (-?[0-9]*) , ([0-9]*) , (.*?) \) (.+)")

(defrecord CombatToggle [event-time in-combat])
(defrecord CombatData [event-time event-type actor-id target-id actor-owner-id target-owner-id actor-name target-name amount spell-id spell-name text])

(defn parse-time [s]
  (let [[_ hr min sec] (re-matches time-re s)]
    (+ (* (Integer/parseInt hr) 60 60) (* (Integer/parseInt min) 60) (Integer/parseInt sec))))

(defn parse-combat-event [event-time data]
  (let [[_ event-type actor-id target-id actor-owner-id target-owner-id actor-name target-name amount spell-id spell-name text] (re-matches combat-event-re data)]
    (->CombatData event-time (int-to-event-type (Integer/parseInt event-type)) actor-id target-id actor-owner-id target-owner-id actor-name target-name (Integer/parseInt amount) (Integer/parseInt spell-id) spell-name text)))

(defn parse-line-data [event-time data]
  (let [[_ state] (re-matches combat-toggle-re data)]
    (if (nil? state)
      (parse-combat-event event-time data)
      (->CombatToggle event-time (= state "Begin")))))

(defn parse-line [line]
  (let [[_ event-time data] (re-matches line-re line)]
    (parse-line-data (parse-time event-time) data)))

(defn parse-lines [lines]
  (pmap parse-line lines))

(defn parse [reader]
  (parse-lines (line-seq reader)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Entity ID Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def entity-re #"T=([NPX])#R=([CGORX])#([0-9]+)")

(defn unpack-entity-id [id] (rest (re-matches entity-re id)))

(defn npc? [id]
  (let [[t r i] (unpack-entity-id id)]
    (= t "N")))

(defn pc? [id]
  (let [[t r i] (unpack-entity-id id)]
    (= t "P")))

(defn nobody? [id]
  (let [[t r i] (unpack-entity-id id)]
    (= t "X")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Event Processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- update-entities [entities event]
  (assoc entities (:actor-id event) (:actor-name event) (:target-id event) (:target-name event)))

(defn map-entities [events]
  (loop [entities {}
         events events]
    (if (empty? events)
      entities
      (recur (update-entities entities (first events)) (rest events)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Fight Splitting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- hostile-action? [event]
  (contains? #{:direct-damage, :damage-over-time, :debuff-gain, :miss, :dodge, :parry, :resist, :crit-damage} (:event-type event)))

(defn- dead-entity [event]
  (let [event-type (:event-type event)]
    (if (= :died event-type)
      (:actor-id event)
      (if (= :slain event-type)
        (:target-id event)
        nil))))

(defn extract-npcs [event]
  (filter npc? [(:actor-id event) (:target-id event)]))

(defn extract-pcs [event]
  (filter pc? [(:actor-id event) (:target-id event)]))

(defrecord DeathEvent [event-time event-type entity-id original-event])

(defn insert-death-later [death events]
  (let [part (partition-by (fn [event] (<= (:event-time event) (:event-time death))) events)]
    (concat (vec (conj (first part) (->DeathEvent (:event-time death) :death (dead-entity death) death))) (second part))))

(defn split-fights [events]
  (loop [fights []
         current-fight []
         npcs #{}
         dead-npcs #{}
         pcs #{}
         dead-pcs #{}
         event (first events)
         events (rest events)]
    (if (nil? event)
      (if (seq current-fight)
        (conj fights current-fight)
        fights)
      (if (= :death (:event-type event))
        (if (npc? (:entity-id event))
          (recur fights (conj current-fight (:original-event event)) npcs (conj dead-npcs (:entity-id event)) pcs dead-pcs (first events) (rest events))
          (recur fights (conj current-fight (:original-event event)) npcs dead-npcs pcs (conj dead-pcs (:entity-id event)) (first events) (rest events)))
        (if (or (and (seq npcs) (= npcs dead-npcs) (seq current-fight))
              (and (seq pcs) (= pcs dead-npcs) (seq current-fight))
              (and (seq current-fight) (>= 5 (- (:event-time event) (:event-time (last current-fight))))))
          (recur (conj fights current-fight) [] #{} #{} #{} #{} (first events) (rest events))
          (if (contains? #{:died :slain} (:event-type event))
            (let [events (insert-death-later event (rest events))]
              (recur fights current-fight npcs dead-npcs pcs dead-pcs (first events) (rest events)))
            (if (or (hostile-action? event) (seq current-fight))
              (recur fights (conj current-fight event) (concat npcs (extract-npcs event)) dead-npcs (concat pcs (extract-pcs event)) dead-pcs (first events) (rest events))
              (recur fights current-fight npcs dead-npcs pcs dead-pcs (first events) (rest events)))))))))
