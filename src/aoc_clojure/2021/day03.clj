(ns aoc-clojure.2021.day03
  (:require [clojure.string :as str]))
(use 'clojure.java.io)

(defn get-lines [fname]
  (with-open [r (reader fname)]
    (doall (line-seq r))))

(defn p1 []
  (let [lines (get-lines "data/2021/d3")
        n (count (first lines))
        stats (loop [state (reduce #(assoc %1 %2 [0 0]) {} (range n))
                     [curr & rest] lines]
                (if curr
                  (recur
                    (reduce
                      #(let [s %1
                             [i v] %2]
                         (assoc s i (map + (s i) (if (= v \0) [1 0] [0 1])))
                         )
                      state
                      (map-indexed list curr)
                      )
                    rest)
                  state)

                )]
    (*
      (Integer/parseInt
        (reduce str (map #(let [[a b] (stats %)] (if (> a b) \0 \1)) (range n))) 2)
      (Integer/parseInt
        (reduce str (map #(let [[a b] (stats %)] (if (> a b) \1 \0)) (range n))) 2)

      )))

(defn count-stats[lines n position]
  ((loop [state (reduce #(assoc %1 %2 [0 0]) {} (range n))
               [curr & rest] lines]
          (if curr
            (recur
              (reduce
                #(let [s %1
                       [i v] %2]
                   (assoc s i (map + (s i) (if (= v \0) [1 0] [0 1])))
                   )
                state
                (map-indexed list curr)
                )
              rest)
            state)

          ) position))

(defn nth-bit[bstr n]
  (nth bstr n))

(defn filter-nums[nums n bit]
  (filter #(= bit (nth-bit % n)) nums))

(defn p2 []
  (let [lines (get-lines "data/2021/d3")
        n (count (first lines))]

    (loop [[c & rest] (range n)
           remainingOxy lines
           remainingCO2 lines]
      (if c
        (let [[a b] (count-stats remainingOxy n c)
              oxyBit (if (> a b) \0 \1)
              [x y] (count-stats remainingCO2 n c)
              co2Bit (if (< y x) \1 \0)]
            (recur
              rest
              (if (= 1 (count remainingOxy)) remainingOxy (filter-nums remainingOxy c oxyBit))
              (if (= 1 (count remainingCO2)) remainingCO2 (filter-nums remainingCO2 c co2Bit))
              )
          )
        (reduce * (map #(Integer/parseInt % 2) [(first remainingOxy) (first remainingCO2)]))



      )

    )))


(p1)

(p2)