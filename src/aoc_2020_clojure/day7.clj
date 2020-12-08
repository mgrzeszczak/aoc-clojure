(ns aoc-2020-clojure.day7
  (:require [clojure.set :as set])
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(defn parse-content [c]
  (list (Integer/parseInt (first c)) (apply str (rest c))))
(defn parse-line [line]
  (let [[_ a b] (re-find #"(\w+) (\w+) bags contain" line)
        rest (re-seq #"(\d+) (\w+) (\w+) bags?" line)]
    (list (str a b) (map (comp parse-content #(drop 1 %)) rest))))

(defn prepare-data [file]
  (as-> (slurp file) x
        (str/split x #"\n")
        (map parse-line x)))

; part 1
(defn can-contain-r? [fn start target data]
  (if (= start target)
    true
    (if (empty? (data start))
      false
      (some identity (map #(fn fn (second %) target data) (data start))))))

(def can-contain?
  (let [memfn (memoize can-contain-r?)]
    (partial memfn memfn)))

(defn solve-p1 [data]
  (let [ks (filter #(not= "shinygold" %) (keys data))]
    (filter #(can-contain? % "shinygold" data) ks)))

(time (as-> (prepare-data "data/input_d7") x
            (into {} (map vec x))
            (solve-p1 x)
            (count x)))


; part 2
(defn solve-p2 [current data]
  (let [bags (data current)]
    (inc (reduce + (map #(* (first %) (solve-p2 (second %) data)) bags)))))

(as-> (prepare-data "data/input_d7") x
      (into {} (map vec x))
      (dec (solve-p2 "shinygold" x)))