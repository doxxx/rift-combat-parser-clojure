(ns net.doxxx.riftcombatparser.parser
  (:use net.doxxx.riftcombatparser.util)
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

(defmethod pprint-str CombatData [x]
  (str (:event-time x) ": " (:actor-name x) " -> " (:target-name x) " -- " (:spell-name x)))

(defn parse-time [s]
  (let [[_ hr min sec] (re-matches time-re s)]
    (+ (* (Integer/parseInt hr) 60 60) (* (Integer/parseInt min) 60) (Integer/parseInt sec))))

(defn pprint-time [t]
  (str (int (/ t (* 60 60))) ":" (int (/ (rem t (* 60 60)) 60)) ":" (int (rem (rem t (* 60 60)) 60))))

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

(defn- compare-entity-type [id expected-type]
  (if (nil? id)
    nil
    (let [[t r i] (unpack-entity-id id)]
      (= t expected-type))))

(defn npc? [id]
  (compare-entity-type id "N"))

(defn pc? [id]
  (compare-entity-type id "P"))

(defn nobody? [id]
  (compare-entity-type id "X"))

(defn grouped? [id]
  (if (nil? id)
    nil
    (let [[t r i] (unpack-entity-id id)]
      (or (= r "C") (= r "G") (= r "R")))))

(defn pet? [id owner-id]
  (and (pc? owner-id) (npc? id)))

(defn npc-not-pet? [id owner-id]
  (when (and (npc? id) (not (pet? id owner-id))) #{id}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Event Processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- map-entity [event]
  (assoc {} (:actor-id event) (:actor-name event) (:target-id event) (:target-name event)))

(defn map-entities [events]
  (reduce merge (pmap map-entity events)))

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
  (assoc event :event-time new-time))

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

(def hostile-event-types #{:direct-damage, :damage-over-time, :debuff-gain, :miss, :dodge, :parry, :resist, :crit-damage})
(def ignored-hostile-spells #{"Sacrifice Life: Mana" "Critter Killer"})

(defn- hostile-action? [event]
  (and (contains? hostile-event-types (:event-type event))
    (not (contains? ignored-hostile-spells (:spell-name event)))
    (not (and (pc? (:actor-id event)) (pc? (:target-id event))))))

(defn- get-dead-entity [event]
  (let [event-type (:event-type event)]
    (if (= :died event-type)
      [(:actor-id event) (:actor-owner-id event)]
      (if (= :slain event-type)
        [(:target-id event) (:target-owner-id event)]
        nil))))

(defn extract-npcs [event]
  (cs/union
    (npc-not-pet? (:actor-id event) (:actor-owner-id event))
    (npc-not-pet? (:target-id event) (:target-owner-id event))))

(defn extract-pcs [event]
  (set (filter pc? [(:actor-id event) (:target-id event)])))

(defrecord DeathEvent [event-time event-type entity-id entity-owner-id original-event])

(defn insert-death-later [death events]
  (let [death-time (:event-time death)
        f (fn [e] (<= (:event-time e) death-time))
        prefix (take-while f events)
        suffix (drop-while f events)
        [dead-entity dead-entity-owner] (get-dead-entity death)]
    (concat prefix [(->DeathEvent death-time :death dead-entity dead-entity-owner death)] suffix)))

(defn time-since-last-event [events event]
  (- (:event-time event) (:event-time (peek events))))

(defn fight-duration [events]
  (- (:event-time (peek events)) (:event-time (first events))))

(defn fight-end? [event current-fight npcs dead-npcs pcs dead-pcs]
  (if (seq current-fight)
    (if (and (seq npcs) (= npcs dead-npcs))
      (do
        (debug-log (str (:event-time event) ": All active NPCs died; ending fight (" (fight-duration current-fight) "s)"))
        true)
      (if (and (seq pcs) (= pcs dead-pcs))
        (do
          (debug-log (str (:event-time event) ": All active PCs died; ending fight (" (fight-duration current-fight) "s)"))
          true)
        (if (>= (time-since-last-event current-fight event) 5)
          (do
            (debug-log (str (:event-time event) ": 5 second timeout; ending fight (" (fight-duration current-fight) "s)"))
            (debug-log (str "Active NPCs: " (pprint-str npcs)))
            (debug-log (str "Dead NPCs: " (pprint-str dead-npcs)))
            (debug-log (str "Active PCs: " (pprint-str pcs)))
            (debug-log (str "Dead PCs: " (pprint-str dead-pcs)))
            true)
          false)))
    false))

(defn valid-action? [event]
  (and
    (= CombatData (type event))
    (or
      (contains? #{:slain :died :power-gain} (:event-type event))
      (not-empty (:spell-name event)))
    (or
      (grouped? (:actor-id event))
      (grouped? (:target-id event))
      (npc? (:actor-id event))
      (npc? (:target-id event)))))

(defn split-fights [all-events]
  (loop [fights []
         current-fight []
         npcs #{}
         dead-npcs #{}
         pcs #{}
         dead-pcs #{}
         events (filter valid-action? all-events)]
    (if (empty? events)
      (if (seq current-fight)
        (conj fights current-fight)
        fights)
      (let [event (first events)
            event-type (:event-type event)]
        (if (= :death event-type)
          (let [dead-entity (:entity-id event)
                dead-entity-owner (:entity-owner-id event)
                original-event (:original-event event)]
            (if (npc? dead-entity)
              (do
                (if (pet? dead-entity dead-entity-owner)
                  (do
                    (debug-log (str (:event-time event) ": Processing Pet death: " dead-entity))
                    (recur fights (conj current-fight original-event) (cs/union npcs (extract-npcs original-event)) dead-npcs (cs/union pcs (extract-pcs original-event)) dead-pcs (rest events)))
                  (do
                    (debug-log (str (:event-time event) ": Processing NPC death: " dead-entity))
                    (recur fights (conj current-fight original-event) (cs/union npcs (extract-npcs original-event)) (conj dead-npcs dead-entity) (cs/union pcs (extract-pcs original-event)) dead-pcs (rest events)))))
              (do
                (debug-log (str (:event-time event) ": Processing PC death: " dead-entity))
                (recur fights (conj current-fight original-event) (cs/union npcs (extract-npcs original-event)) dead-npcs (cs/union pcs (extract-pcs original-event)) (conj dead-pcs dead-entity) (rest events)))))
          (if (fight-end? event current-fight npcs dead-npcs pcs dead-pcs)
            (recur (conj fights current-fight) [] #{} #{} #{} #{} events)
            (if (or (= :died event-type) (= :slain event-type))
              (recur fights current-fight npcs dead-npcs pcs dead-pcs (insert-death-later event (rest events)))
              (if (or (hostile-action? event) (seq current-fight))
                (do
                  (when (empty? current-fight)
                    (debug-log (str (:event-time event) ": First hostile action: " (pprint-str event))))
                  (recur fights (conj current-fight event) (cs/union npcs (extract-npcs event)) dead-npcs (cs/union pcs (extract-pcs event)) dead-pcs (rest events)))
                (recur fights current-fight npcs dead-npcs pcs dead-pcs (rest events))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Fight Processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def damage-event-types #{:direct-damage :damage-over-time :crit-damage})

(defn extract-npc-damage [event]
  (when (and (npc? (:target-id event)) (contains? damage-event-types (:event-type event)))
    [(:target-name event) (:amount event)]))

(defn total-damage [m [name dmg]]
  (if (nil? name) m (conj m [name (+ dmg (m name 0))])))

(defn primary-npc [events]
  (first (last (sort-by (fn [[name dmg]] dmg) (reduce total-damage {} (map extract-npc-damage events))))))