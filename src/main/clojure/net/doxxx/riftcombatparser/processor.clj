(ns net.doxxx.riftcombatparser.processor
  (:use net.doxxx.riftcombatparser.util)
  (:use net.doxxx.riftcombatparser.parser)
  (:import net.doxxx.riftcombatparser.parser.CombatData)
  (:require [clojure.set :as cs]))

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
