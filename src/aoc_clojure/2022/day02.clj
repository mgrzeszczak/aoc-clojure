(ns aoc-clojure.2022.day02
  (:require [clojure.string :as str]))
(use 'clojure.java.io)

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split x #"\n")
        (map #(str/split % #" ") x)
        x))

(def scores {:win 6 :draw 3 :loss 0})
(def shape-scores {:rock 1 :paper 2 :scissors 3})
(def shapes {"A" :rock "B" :paper "C" :scissors "X" :rock "Y" :paper "Z" :scissors})

(def outcomes {"X" :loss "Y" :draw "Z" :win})

(def shape-sequence [:rock :paper :scissors])
(def shape-index {:rock 0 :paper 1 :scissors 2})

(def shape-win {:rock :scissors :paper :rock :scissors :paper})

(defn outcome[a b]
  (cond
    (= a b) :draw
    (= (shape-win b) a) :win
    :else :loss
    ))

(defn round[[s1 s2]]
  (+ (scores (outcome s1 s2)) (shape-scores s2)))

(defn solve-p1 []
  (as-> (parse-input "data/2022/d2") x
        (map (fn [[a b]] [(shapes a) (shapes b)]) x)
        (reduce + (map round x))))

(defn choose-shape[shape outcome]
  (case outcome
        :win (shape-sequence (mod (inc (shape-index shape)) 3))
        :draw shape
        :loss (shape-sequence (mod (dec (shape-index shape)) 3))
        )
  )

(defn solve-p2[]
  (as-> (parse-input "data/2022/d2") x
        (map (fn [[a b]] [(shapes a) (outcomes b)]) x)
        (map (fn [[shape outcome]] [shape (choose-shape shape outcome)]) x)
        (reduce + (map round x)))
  )

(solve-p1)
(solve-p2)