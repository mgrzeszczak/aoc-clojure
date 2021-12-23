(ns aoc-clojure.2021.day21
  (:require [clojure.string :as str]
            [clojure.set :as sets]
            [aoc-clojure.lib.lib :as lib]))
(use 'clojure.java.io)
(require '[clojure.data.priority-map :refer [priority-map]])

(defn parse-position [line]
  (dec (Integer/parseInt (second (first (re-seq #"Player \d+ starting position: (\d+)" line))))))

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split-lines x)
        (map parse-position x)))

(defn roll [dice]
  [(inc (mod dice 100)) (inc dice)])

(defn roll-n [dice n]
  (reduce (fn [[s d] _]
            (let [[v n] (roll d)]
              [(+ s v) n])) [0 dice] (range n)))

(defn move [pos dice]
  (let [[v d] (roll-n dice 3)
        n-pos (mod (+ pos v) 10)]
    [n-pos (inc n-pos) d]))

(time (as-> (parse-input "data/2021/d21") x
            (let [[p1 p2] x]
              (loop [p1-score 0
                     p2-score 0
                     dice 0
                     p1 p1
                     p2 p2
                     turn 0
                     who :p1]
                (if (>= p1-score 1000)
                  ["p1 wins" (* p2-score (* turn 3))]
                  (if (>= p2-score 1000)
                    ["p2 wins" (* p1-score (* turn 3))]
                    (if (= who :p1)
                      (let [[np ip d] (move p1 dice)]
                        (recur (+ p1-score ip) p2-score d np p2 (inc turn) :p2)
                        )
                      (let [[np ip d] (move p2 dice)]
                        (recur p1-score (+ p2-score ip) d p1 np (inc turn) :p1)
                        ))))))))

(def dice-states
  (map (fn [[k v]] [k (count v)]) (group-by identity (for [x [1 2 3]
                                                           y [1 2 3]
                                                           z [1 2 3]]
                                                       (+ x y z))))
  )

(defn move-p2 [pos value]
  (let [n-pos (mod (+ pos value) 10)]
    [n-pos (inc n-pos)]))

(defn vec* [v value]
  (vec (map #(* value %) v)))

(defn vec+ [a b]
  (vec (map #(+ %1 %2) a b)))

(defn new-state [[p1-pos p2-pos p1-score p2-score turn] rolled]
  (if (= turn :p1)
    (let [[p s] (move-p2 p1-pos rolled)]
      [p p2-pos (+ p1-score s) p2-score :p2]
      )
    (let [[p s] (move-p2 p2-pos rolled)]
      [p1-pos p p1-score (+ p2-score s) :p1]
      )))

(defn part2
  ([state] (reduce max (first (part2 state {}))))
  ([[p1-pos p2-pos p1-score p2-score turn :as state] cache]
   (if (cache state)
     [(cache state) cache]
     (let [[res-cache res] (cond
                             (>= p1-score 21) [cache [1 0]]
                             (>= p2-score 21) [cache [0 1]]
                             :else
                             (reduce (fn [[cache aggr] [roll cnt]]
                                       (let [[res new-cache] (part2 (new-state state roll) cache)]
                                         [new-cache (vec+ aggr (vec* res cnt))]
                                         )) [cache [0 0]] dice-states)

                             )]
       [res (assoc res-cache state res)]
       ))))

(time (as-> (parse-input "data/2021/d21") x
            (let [[p1 p2] x]
              (part2 [p1 p2 0 0 :p1])
              )))