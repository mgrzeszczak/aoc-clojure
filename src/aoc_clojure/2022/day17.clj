(ns aoc-clojure.2022.day17
  (:require [aoc-clojure.lib.lib :as lib]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))
(use 'clojure.java.io)

(defn parse-line [line]
  (let [[_ valve flow-rate valves] (first (re-seq #"Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.+)" line))]
    [valve {:flow-rate (Integer/parseInt flow-rate) :valves (str/split valves #", ")}]))

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split x #"")
        ))

(def min-x -3)
(def max-x 3)

(def floor-y 0)
(def new-rock-offset 4)

(def rock1 [[-1 0] [0 0] [1 0] [2 0]])
(def rock2 [[-1 1] [0 1] [1 1] [0 0] [0 2]])
(def rock3 [[-1 0] [0 0] [1 0] [1 1] [1 2]])
(def rock4 [[-1 0] [-1 1] [-1 2] [-1 3]])
(def rock5 [[-1 0] [0 0] [-1 1] [0 1]])
(def rocks [rock1 rock2 rock3 rock4 rock5])

(def floor [[-3 0] [-2 0] [-1 0] [0 0] [1 0] [2 0] [3 0]])

(defn create-rock [rock-no h]
  (vec (map (fn [[x y]] [x (+ y h new-rock-offset)]) (rocks rock-no))))

(defn validate-rock [rock stopped-rocks]
  (not
    (or
      (some (fn [[x _]] (or (< x min-x) (> x max-x))) rock)
      (some stopped-rocks rock))))

(defn move-rock [rock dir]
  (let [[dx dy] (case dir
                  "<" [-1 0]
                  ">" [1 0]
                  :down [0 -1]
                  )]
    (vec (map (fn [[x y]] [(+ x dx) (+ y dy)]) rock))
    ))

(defn next-rock [rock-no h]
  [(create-rock rock-no h) (mod (inc rock-no) 5)])

(defn simulate-fall [rock stopped-rocks move-no jet-pattern]
  (loop [move move-no
         r rock
         move-type :jet]
    (let [after-move (move-rock r (case move-type :jet (jet-pattern move) :fall :down))
          valid (validate-rock after-move stopped-rocks)]
      (if (and (not valid) (= move-type :fall))
        [r move]
        (recur
          (if (= move-type :fall) move (rem (inc move) (count jet-pattern)))
          (if valid after-move r)
          (case move-type :jet :fall :fall :jet))
        )
      )
    )
  )

(defn extract-top-rows [stopped-rocks h n]
  (let [rows (filter (fn [[_ y]] (<= (- h y) n)) stopped-rocks)
        min-y (apply min (map second rows))]
    (into #{} (map (fn[[x y]] [x (- y min-y)]) rows))
    ))


(defn p1 [jet-pattern how-many-rocks]
  (loop [stopped-rocks (into #{} floor)
         move-no 0
         rock-no 0
         rock-count 0
         h 0]
    (if (= how-many-rocks rock-count)
      h
      (let [[rock new-rock-no] (next-rock rock-no h)
            [after-move next-move-no] (simulate-fall rock stopped-rocks move-no jet-pattern)
            rock-h (apply max (map second after-move))
            ]
        (recur
          (into stopped-rocks after-move)
          next-move-no
          new-rock-no
          (inc rock-count)
          (max h rock-h)
          )
        )
      )
    )
  )

(defn find-cycle [jet-pattern n]
  (loop [stopped-rocks (into #{} floor)
         move-no 0
         rock-no 0
         rock-count 0
         history {}
         h 0]
    ;(if (= 0 (mod rock-count 100))
    ;  (println rock-count (count history))
    ;  )
    (let [state [(extract-top-rows stopped-rocks h n) move-no rock-no]]
      (if (history state)
        [(history state) [rock-count h]]

        (let [
              [rock new-rock-no] (next-rock rock-no h)
              [after-move next-move-no] (simulate-fall rock stopped-rocks move-no jet-pattern)
              rock-h (apply max (map second after-move))
              new-history (if
                            (>= h n)
                            (assoc history state [rock-count h])
                            history
                            )
              ]
          (recur
            (into stopped-rocks after-move)
            next-move-no
            new-rock-no
            (inc rock-count)
            new-history
            (max h rock-h)
            )
          )

        )
      )


    )
  )

(def target-p2 1000000000000)

(defn p2[jet-pattern window]
  (let [[[count-a h-a] [count-b h-b]] (find-cycle jet-pattern window)
        loops (quot (- target-p2 count-a) (- count-b count-a))
        remaining (rem (- target-p2 count-a) (- count-b count-a))
        start-h (p1 jet-pattern (+ count-a remaining))
        ]
    (+ start-h (* loops (- h-b h-a)))
    )
  )

(defn solve []
  (let [jet-pattern (parse-input "data/2022/d17")]
    [(p1 jet-pattern 2022)
    (p2 jet-pattern 10)]
    )
  )

(time (solve))