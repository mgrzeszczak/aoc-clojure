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
(defn calculate [expr startIndex]
  (loop [i startIndex
         op nil
         curr nil]
    (if (>= i (count expr))
      (list curr i)
      (let [symbol (expr i)]
        (case symbol
          "(" (let [[v ind] (calculate expr (inc i))]
                (recur ind nil
                       (case op "*" (* curr v) "+" (+ curr v) v))
                )
          ")" (list curr (inc i))
          "+" (recur (inc i) "+" curr)
          "*" (recur (inc i) "*" curr)
          (recur (inc i) nil (case op "*" (* curr symbol) "+" (+ curr symbol) symbol)))))))

(time (as-> (parse-input "data/input_d18") x
            (reduce + (map #(first (calculate % 0)) x))))

; part 2

(defn calc-term [input i]
  (let [[number ni] (calc-number input i)]
    (loop [value number
           pos ni]
      (if (>= pos (count input))
        (list value pos)
        (let [token (input pos)]
          (if (= token "+")
            (let [[numberValue newIndex] (calc-number input (inc pos))]
              (recur (+ numberValue value) newIndex))
            (list value pos)))))))

(defn calc-number [input i]
  (let [token (input i)]
    (if (= token "(")
      (let [[exp ni] (calc-expression input (inc i))]
        (list exp (inc ni)))
      (list token (inc i)))))

(defn calc-expression [input i]
  (let [[term ni] (calc-term input i)]
    (loop [value term
           pos ni]
      (if (>= pos (count input))
        (list value pos)
        (let [token (input pos)]
          (if (= token "*")
            (let [[termValue newIndex] (calc-term input (inc pos))]
              (recur (* value termValue) newIndex))
            (list value pos)))))))

(time (as-> (parse-input "data/input_d18") x
            (reduce + (map #(first (calc-expression % 0)) x))))