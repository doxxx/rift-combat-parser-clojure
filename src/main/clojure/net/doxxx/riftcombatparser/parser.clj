(ns net.doxxx.riftcombatparser.parser
  (:use net.doxxx.riftcombatparser.util)
  (:require [clojure.set :as cs]))

(set! *warn-on-reflection* true)

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

(defrecord CombatToggle [event-time in-combat])
(defrecord CombatData [event-time event-type actor-id target-id actor-owner-id target-owner-id actor-name target-name amount spell-id spell-name text])

(defmethod pprint-str CombatData [x]
  (str (:event-time x) ": " (:actor-name x) " -> " (:target-name x) " -- " (:spell-name x)))

(defn calc-time [hr min sec]
  (+ (* hr 60 60) (* min 60) sec))

(defn parse-time
  ([hr min sec]
    (calc-time (Integer/parseInt hr) (Integer/parseInt min) (Integer/parseInt sec)))
  ([^String s]
    (parse-time (subs s 0 2) (subs s 3 5) (subs s 6 8))))

(defn pprint-time [t]
  (str (int (/ t (* 60 60))) ":" (int (/ (rem t (* 60 60)) 60)) ":" (int (rem (rem t (* 60 60)) 60))))

(defn split-time-data [^String s]
  [(subs s 0 8) (subs s 8)])

(defn split-combat-event [^String s]
  (loop [elements []
         s s]
    (let [i (.indexOf s " , ")]
      (if (>= i 0)
        (recur (conj elements (subs s 0 i)) (subs s (+ i 3)))
        (conj elements s)))))

(defn parse-combat-event [^String time ^String data]
  (let [[event-type actor-id target-id actor-owner-id target-owner-id actor-name target-name amount
         spell-id spell-name] (split-combat-event (subs data 4 (- (.indexOf data (int \))) 1)))
        text (subs data (+ (.indexOf data (int \))) 2))]
    (->CombatData (parse-time time) (int-to-event-type (Integer/parseInt event-type)) actor-id target-id
      actor-owner-id target-owner-id actor-name target-name (Integer/parseInt amount) (Integer/parseInt spell-id)
      spell-name text)))

(defn parse-line [^String line]
  (let [[time data] (split-time-data line)]
    (if (= data " Combat Begin")
      (->CombatToggle (parse-time time) true)
      (if (= data " Combat End")
        (->CombatToggle (parse-time time) false)
        (parse-combat-event time data)))))

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
