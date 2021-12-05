(ns aoc-clojure.2020.day05
  (:require [clojure.set :as set])
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(defn str2char [s] (first (char-array s)))

(defn with-file [fname fn]
  (with-open [r (io/reader fname)]
    (do (fn (line-seq r)))))

(defn parse-seat [[f & rest] lb ub]
  (if (not f)
    lb
    (let [r (/ (inc (- ub lb)) 2)]
      (cond
        ((set '("F" "L")) f) (recur rest lb (- ub r))
        ((set '("B" "R")) f) (recur rest (+ lb r) ub)))))

(defn seat-number [code]
  (let [s (str/split code #"")]
    (+ (* 8 (parse-seat (drop-last 3 s) 0 127))
       (parse-seat (take-last 3 s) 0 7))))

; part 1
(time (with-file "data/2020/input_d05"
                 (fn [lines]
                   (->> (do lines)
                        (map seat-number)
                        (reduce max)))))


; part 2
(with-file "data/2020/input_d05"
           (fn [lines]
             (let [s (set (->> (do lines)
                               (map seat-number)))
                   r (range (apply min s) (inc (apply max s)))]
               (first (set/difference (set r) s)))))