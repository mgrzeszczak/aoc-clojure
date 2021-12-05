(ns aoc-clojure.2020.day08
  (:require [clojure.set :as set])
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))


; part 1
(defn solve-p1 [pos acc visited instr]
  (if (visited pos)
    acc
    (let [[i v] (instr pos)]
      (case i
        "nop" (solve-p1 (inc pos) acc (conj visited pos) instr)
        "jmp" (solve-p1 (+ pos v) acc (conj visited pos) instr)
        "acc" (solve-p1 (inc pos) (+ acc v) (conj visited pos) instr)))))

(time (as-> (slurp "data/2020/input_d08") x
            (str/split-lines x)
            (map #(re-matches #"(acc|jmp|nop) ([+-]\d+)" %) x)
            (map #(list (second %) (Integer/parseInt (nth % 2))) x)
            (into [] x)
            (solve-p1 0 0 #{} x)))

; part 2
(defn solve-p2 [pos acc visited instr]
  (if (or (>= pos (count instr)) (visited pos))
    (list (>= pos (count instr)) acc)
    (let [[i v] (instr pos)]
      (case i
        "nop" (solve-p2 (inc pos) acc (conj visited pos) instr)
        "jmp" (solve-p2 (+ pos v) acc (conj visited pos) instr)
        "acc" (solve-p2 (inc pos) (+ acc v) (conj visited pos) instr)))))

(defn brute-force-p2 [p instr]
  (let [[i v] (instr p)]
    (if (#{"nop" "jmp"} i)
      (let [[success acc] (solve-p2 0 0 #{} (assoc instr p (list ({"nop" "jmp" "jmp" "nop"} i) v)))]
        (if success
          acc
          (recur (inc p) instr)))
      (recur (inc p) instr))))

(time (as-> (slurp "data/2020/input_d08") x
            (str/split-lines x)
            (map #(re-matches #"(acc|jmp|nop) ([+-]\d+)" %) x)
            (map #(list (second %) (Integer/parseInt (nth % 2))) x)
            (into [] x)
            (brute-force-p2 0 x)))