(ns net.doxxx.riftcombatparser.parser
  (:require [clojure.set :as cs]))

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

(def int-to-event-type (cs/map-invert event-types))

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

(defn- rel-time [event start-time]
  (let [t (- (:event-time event) start-time)]
    (if (< t 0)
      (+ t 86400)
      t)))

(def ^:dynamic *start-time* 0)

(defn- normalize-event-time [event]
  (merge event (assoc (apply hash-map (interleave (keys event) (vals event))) :event-time (rel-time event *start-time*))))

(defn normalize-event-times [events]
  (binding [*start-time* (:event-time (first events))]
    (map normalize-event-time events)))

(defn update-event-time [event new-time]
  (merge event {:event-time new-time}))

(defn normalize-event-times [events]
  (loop [prev-event nil current-event (first events) rest-events (rest events) result [] offset (- (:event-time (first events)))]
    (if (nil? current-event)
      result
      (let [event-time (:event-time current-event)]
        (let [offset (if (nil? prev-event)
                       offset
                       (if (< event-time (:event-time prev-event))
                         (+ offset 86400)
                         offset))]
          (recur current-event (first rest-events) (rest rest-events) (conj result (update-event-time current-event (+ event-time offset))) offset))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Fight Splitting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def hostile-actions #{:direct-damage, :damage-over-time, :debuff-gain, :miss, :dodge, :parry, :resist, :crit-damage})

(defn- hostile-action? [event]
  (contains? hostile-actions (:event-type event)))

(defn- dead-entity [event]
  (let [event-type (:event-type event)]
    (if (= :died event-type)
      (:actor-id event)
      (if (= :slain event-type)
        (:target-id event)
        nil))))

(defn extract-npcs [event]
  (set (filter npc? [(:actor-id event) (:target-id event)])))

(defn extract-pcs [event]
  (set (filter pc? [(:actor-id event) (:target-id event)])))

(defrecord DeathEvent [event-time event-type entity-id original-event])

(defn insert-death-later [death events]
  (let [death-time (:event-time death)
        f (fn [e] (<= (:event-time e) death-time))
        prefix (take-while f events)
        suffix (drop-while f events)]
    (concat prefix [(->DeathEvent death-time :death (dead-entity death) death)] suffix)))

(defn time-since-last-event [events event]
  (- (:event-time event) (:event-time (peek events))))

(defn fight-end? [event current-fight npcs dead-npcs pcs dead-pcs]
  (if (seq current-fight)
    (if (and (seq npcs) (= npcs dead-npcs))
      (do
        (println (str (:event-time event) ": All active NPCs died; ending fight"))
        true)
      (if (and (seq pcs) (= pcs dead-pcs))
        (do
          (println (str (:event-time event) ": All active PCs died; ending fight"))
          true)
        (let [timeout (time-since-last-event current-fight event)]
          (if (>= timeout 5)
            (do
              (println (str (:event-time event) ": 5 second timeout; ending fight"))
              true)
            (do
;              (println (str (:event-time event) ": not enough time since last event: " timeout))
              false)))))
    false))

(defn split-fights [all-events]
  (loop [fights []
         current-fight []
         npcs #{}
         dead-npcs #{}
         pcs #{}
         dead-pcs #{}
         events all-events]
    (if (empty? events)
      (if (seq current-fight)
        (conj fights current-fight)
        fights)
      (let [event (first events)
            event-type (:event-type event)]
        (if (= :death event-type)
          (let [dead-entity (:entity-id event)
                original-event (:original-event event)]
            (if (npc? dead-entity)
              (do
                (println (str (:event-time event) ": Processing NPC death: " dead-entity))
                (recur fights (conj current-fight original-event) (cs/union npcs (extract-npcs original-event)) (conj dead-npcs dead-entity) (cs/union pcs (extract-pcs original-event)) dead-pcs (rest events)))
              (do
                (println (str (:event-time event) ": Processing PC death: " dead-entity))
                (recur fights (conj current-fight original-event) (cs/union npcs (extract-npcs original-event)) dead-npcs (cs/union pcs (extract-pcs original-event)) (conj dead-pcs dead-entity) (rest events)))))
          (if (fight-end? event current-fight npcs dead-npcs pcs dead-pcs)
            (recur (conj fights current-fight) [] #{} #{} #{} #{} events)
            (if (or (= :died event-type) (= :slain event-type))
              (recur fights current-fight npcs dead-npcs pcs dead-pcs (insert-death-later event (rest events)))
              (if (or (hostile-action? event) (seq current-fight))
                (do
                  (when (empty? current-fight)
                    (print (str (:event-time event) ": First hostile action: "))
                    (prn event))
                  (recur fights (conj current-fight event) (cs/union npcs (extract-npcs event)) dead-npcs (cs/union pcs (extract-pcs event)) dead-pcs (rest events)))
                (recur fights current-fight npcs dead-npcs pcs dead-pcs (rest events))))))))))

(defn fight-duration [events]
  (- (:event-time (last events)) (:event-time (first events))))
