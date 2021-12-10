(ns aoc-clojure.2021.day10
  (:require [clojure.string :as str]
            [clojure.set :as sets]))
(use 'clojure.java.io)

(defn parse-line [line]
  (str/split line #""))

(defn parse-input [file-name]
  (->> (slurp file-name)
       (str/split-lines)
       (map parse-line)))

(def points {")" 3 "]" 57 "}" 1197 ">" 25137})
(def closing-brackets {")" "(" "]" "[" "}" "{" ">" "<"})

(defn line-error-score [line]
  (loop [[c & rest] line
         stack '()]
    (if (nil? c)
      [false stack]
      (if (closing-brackets c)
        (if (not= (peek stack) (closing-brackets c))
          [true (points c)] (recur rest (pop stack)))
        (recur rest (conj stack c))))))

(defn p1 [lines]
  (reduce + (map second (filter first (map line-error-score lines)))))

(def compl-scores {"(" 1 "[" 2 "{" 3 "<" 4})

(defn complete-line [stack]
  (reduce #(+ (* %1 5) (compl-scores %2)) 0 stack))

(defn median [xs]
  (let [sorted (vec (sort xs))
        len (count sorted)]
    (sorted (quot len 2))))

(defn p2 [lines]
  (let [incomplete (map second (filter #(not (first %)) (map line-error-score lines)))
        scores (map complete-line incomplete)]
    (median scores)))

; p1
(time (as-> (parse-input "data/2021/d10") x
            (p1 x)))
; p2
(time (as-> (parse-input "data/2021/d10") x
            (p2 x)))