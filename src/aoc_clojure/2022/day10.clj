(ns aoc-clojure.2022.day10
  (:require [clojure.string :as str]
            [aoc-clojure.lib.lib :as lib]))
(use 'clojure.java.io)


(defn parse-line[line]
  (if (= line "noop")
    [:noop]
    (let [[command value](str/split line #" ")]
      [:addx (Integer/parseInt value)])))

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split-lines x)
        (mapcat #(if (= :noop (first %)) [%] [[:noop] %]) (map parse-line x))
        x))


(def cycles #{20 60 100 140 180 220})
(def new-line-cycles #{20 60 100 140 180 220})

(defn solve[]
  (as-> (parse-input "data/2022/d10") x
        (loop [[curr & rem] x
               x 1
               cycle 1
               sum 0
               row 1]
          (if (not curr)
            sum
            (let [strength (* x cycle)
                  new-sum (if (cycles cycle) (+ sum strength) sum)
                  pixel-pos (- (dec cycle) (* 40 (dec row)))
                  pixel (if (<= (lib/abs (- x pixel-pos)) 1) "#" ".")
                  end-of-row (= 0 (mod cycle 40))
                  new-row (if end-of-row (inc row) row)
                  ]
              (print pixel)
              (if end-of-row (print "\n"))
              (if (= :addx (first curr))
                (recur rem (+ x (second curr)) (inc cycle) new-sum new-row)
                (recur rem x (inc cycle) new-sum new-row)
                )
              )
            )


          )
        ))

(solve)