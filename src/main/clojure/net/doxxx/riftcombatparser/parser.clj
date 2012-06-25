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

; groups: hr min sec state
(def combat-toggle-re #"([0-9][0-9]):([0-9][0-9]):([0-9][0-9]) Combat (Begin|End)")
; groups: hr min sec event-type actor-id target-id actor-owner-id target-owner-id actor-name target-name amount spell-id spell-name text
(def combat-event-re #"([0-9][0-9]):([0-9][0-9]):([0-9][0-9]): \( ([0-9]+) , (T=.+) , (T=.+) , (T=.+) , (T=.+) , (.*?) , (.*?) , (-?[0-9]*) , ([0-9]*) , (.*?) \) (.+)")

(defrecord CombatToggle [event-time in-combat])
(defrecord CombatData [event-time event-type actor-id target-id actor-owner-id target-owner-id actor-name target-name amount spell-id spell-name text])

(defmethod pprint-str CombatData [x]
  (str (:event-time x) ": " (:actor-name x) " -> " (:target-name x) " -- " (:spell-name x)))

(defn calc-time [hr min sec]
  (+ (* hr 60 60) (* min 60) sec))

(defn parse-time [hr min sec]
  (calc-time (Integer/parseInt hr) (Integer/parseInt min) (Integer/parseInt sec)))

(defn pprint-time [t]
  (str (int (/ t (* 60 60))) ":" (int (/ (rem t (* 60 60)) 60)) ":" (int (rem (rem t (* 60 60)) 60))))

(defn parse-line [line]
  (let [[_ hr min sec state] (re-matches combat-toggle-re line)]
    (if (nil? state)
      (let [[_ hr min sec event-type actor-id target-id actor-owner-id target-owner-id actor-name target-name amount
             spell-id spell-name text] (re-matches combat-event-re line)]
        (->CombatData (parse-time hr min sec) (int-to-event-type (Integer/parseInt event-type)) actor-id target-id
          actor-owner-id target-owner-id actor-name target-name (Integer/parseInt amount) (Integer/parseInt spell-id)
          spell-name text))
      (->CombatToggle (parse-time hr min sec) (= state "Begin")))))

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
