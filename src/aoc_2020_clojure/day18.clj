(ns aoc-2020-clojure.day18
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-line [line]
  (as-> line x
        (filter #(not= " " %) (str/split x #""))
        (map #(if (re-matches #"\d+" %) (Integer/parseInt %) %) x)
        (vec x)))

(defn parse-input [file]
  (as-> (slurp file) x
        (str/split-lines x)
        (map parse-line x)))

; part 1
(defn calc-number-p1 [input i]
  (let [token (input i)]
    (if (= token "(")
      (let [[exp ni] (calc-expression-p1 input (inc i))]
        (list exp (inc ni)))
      (list token (inc i)))))

(defn calc-expression-p1 [input i]
  (let [[term ni] (calc-number-p1 input i)]
    (loop [value term
           pos ni]
      (if (>= pos (count input))
        (list value pos)
        (let [token (input pos)]
          (if (#{"*" "+"} token)
            (let [[termValue newIndex] (calc-number-p1 input (inc pos))]
              (recur (if (= token "*") (* value termValue) (+ value termValue)) newIndex))
            (list value pos)))))))

(time (as-> (parse-input "data/input_d18") x
            (reduce + (map #(first (calc-expression-p1 % 0)) x))))

; part 2
(defn calc-term-p2 [input i]
  (let [[number ni] (calc-number-p2 input i)]
    (loop [value number
           pos ni]
      (if (>= pos (count input))
        (list value pos)
        (let [token (input pos)]
          (if (= token "+")
            (let [[numberValue newIndex] (calc-number-p2 input (inc pos))]
              (recur (+ numberValue value) newIndex))
            (list value pos)))))))

(defn calc-number-p2 [input i]
  (let [token (input i)]
    (if (= token "(")
      (let [[exp ni] (calc-expression-p2 input (inc i))]
        (list exp (inc ni)))
      (list token (inc i)))))

(defn calc-expression-p2 [input i]
  (let [[term ni] (calc-term-p2 input i)]
    (loop [value term
           pos ni]
      (if (>= pos (count input))
        (list value pos)
        (let [token (input pos)]
          (if (= token "*")
            (let [[termValue newIndex] (calc-term-p2 input (inc pos))]
              (recur (* value termValue) newIndex))
            (list value pos)))))))

(time (as-> (parse-input "data/input_d18") x
            (reduce + (map #(first (calc-expression % 0)) x))))