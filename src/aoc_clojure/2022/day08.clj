(ns aoc-clojure.2022.day08
  (:require [clojure.string :as str]
            [aoc-clojure.lib.lib :as lib]))
(use 'clojure.java.io)


(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split-lines x)
        (lib/parse-grid-2 x #(Integer/parseInt %))
        x))


(defn is-visible [[x y] h grid mx my]
  (or
    (#{0 mx} x)
    (#{0 my} y)
    (every? #(< % h) (map #(grid [% y]) (range 0 x)))
    (every? #(< % h) (map #(grid [x %]) (range 0 y)))
    (every? #(< % h) (map #(grid [% y]) (range (inc x) (inc mx))))
    (every? #(< % h) (map #(grid [x %]) (range (inc y) (inc my))))
    )
  )

(defn scenic-score [[x y] h grid mx my]
  (if (or
        (#{0 mx} x)
        (#{0 my} y))
    0
    (let [view [
                (min (inc (count (take-while #(< (grid [% y]) h) (reverse (range 0 x))))) x)
                (min (inc (count (take-while #(< (grid [x %]) h) (reverse (range 0 y))))) y)
                (min (inc (count (take-while #(< (grid [% y]) h) (range (inc x) (inc mx))))) (- (inc mx) (inc x)))
                (min (inc (count (take-while #(< (grid [x %]) h) (range (inc y) (inc my))))) (- (inc my) (inc y)))
                ]]
      (reduce * view)
      )
    ))

(defn solve []
  (as-> (parse-input "data/2022/d8") x
        (let [mx (reduce max (map first (keys x)))
              my (reduce max (map second (keys x)))]
          [
           (count (filter #(is-visible % (x %) x mx my) (keys x)))
           (reduce max (map #(scenic-score % (x %) x mx my) (keys x)))
           ]
          )
        ))

(solve)
