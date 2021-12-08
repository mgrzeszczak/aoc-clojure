(ns aoc-clojure.2021.day08
  (:require [clojure.string :as str]
            [clojure.set :as sets]))
(use 'clojure.java.io)

(defn parse-line [line]
  (map #(str/split (str/trim %) #"\s+") (str/split line #"\|")))

(defn parse-input [file-name]
  (->> (slurp file-name)
       (str/split-lines)
       (map parse-line)))

(defn resolve-signals [[patterns digits]]
  (let [by-count (group-by count (map #(into #{} %) patterns))
        one (first (by-count 2))
        four (first (by-count 4))
        seven (first (by-count 3))
        eight (first (by-count 7))
        nine (first (filter #(= 1 (count (sets/difference % (sets/union four seven)))) (by-count 6)))
        six (first (filter #(and (not= nine %) (not-empty (sets/difference one %))) (by-count 6)))
        c (first (sets/difference eight six))
        f (first (sets/difference one #{c}))
        zero (first (filter #(and (not= % nine) (not= % six)) (by-count 6)))
        three (first (filter #(empty? (sets/difference one %)) (by-count 5)))
        five (first (filter #(and (not= three %) (% f)) (by-count 5)))
        two (first (filter #(and (not= three %) (not= five %)) (by-count 5)))
        mapping (zipmap [zero one two three four five six seven eight nine] (range 10))]
    (Long/parseLong (str/join (map mapping (map #(into #{} %) digits))))))

; p1
(time (as-> (parse-input "data/2021/d8") x
            (map second x)
            (map #(map count %) x)
            (map #(count (filter (fn [v] (#{2 3 4 7} v)) %)) x)
            (reduce + x)))
; p2
(time (as-> (parse-input "data/2021/d8") x
            (map resolve-signals x)
            (reduce + x)))
