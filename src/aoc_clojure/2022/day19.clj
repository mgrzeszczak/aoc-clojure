(ns aoc-clojure.2022.day19
  (:require [aoc-clojure.lib.lib :as lib]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))
(use 'clojure.java.io)

(defn parse-blueprint [blueprint]
  (as-> (re-seq #"(\d+)" blueprint) x
        (map second x)
        (map #(Integer/parseInt %) x)
        ;(let [[id
        ;       ore-robot-ore-cost
        ;       clay-robot-ore-cost
        ;       obsidian-robot-ore-cost
        ;       obsidian-robot-clay-cost
        ;       geode-robot-ore-cost
        ;       geode-robot-obsidian-cost
        ;       ] x])
        )
  ;(re-matches #"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian." blueprint)
  )

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split-lines x)
        (map parse-blueprint x)
        ))


(defn eval-blueprint [[id
                       ore-robot-ore-cost
                       clay-robot-ore-cost
                       obsidian-robot-ore-cost
                       obsidian-robot-clay-cost
                       geode-robot-ore-cost
                       geode-robot-obsidian-cost]
                      max-time]
  (loop [time 0
         ore 0
         clay 0
         obsidian 0
         geode 0
         ore-robots 1
         clay-robots 0
         obsidian-robots 0
         geode-robots 0]
    (if (= time max-time)
      geode
      (cond
        ; can build ore robot
        (>= ore ore-robot-ore-cost) (recur (inc time)
                                           (- (+ ore ore-robots) ore-robot-ore-cost)
                                           (+ clay clay-robots)
                                           (+ obsidian obsidian-robots)
                                           (+ geode geode-robots)
                                           (inc ore-robots)
                                           clay-robots
                                           obsidian-robots
                                           geode-robots)
        ; can build clay robot
        ; can build obsidian robot
        ; can build geode robot
        )
      )
    )
  )

(defn solve []
  (let [data (parse-input "data/2022/d19")]
    data
    )
  )

; solved in python instead, in day19.py

(time (solve))