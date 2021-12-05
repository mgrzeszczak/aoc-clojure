(ns aoc-clojure.2020.day06
  (:require [clojure.set :as set])
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

; part 1
(as-> (slurp "data/2020/input_d06") x
      (str/split x #"\n\n")
      (map #(str/replace % #"\n" "") x)
      (map #(set (str/split % #"")) x)
      (map count x)
      (reduce + x))

; part 2
(as-> (slurp "data/2020/input_d06") x
      (str/split x #"\n\n")
      (map #(str/split % #"\n") x)
      (map #(map (fn [v] (set (str/split v #""))) %) x)
      (map #(reduce set/intersection %) x)
      (map count x)
      (reduce + x))