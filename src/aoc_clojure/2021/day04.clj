(ns aoc-clojure.2021.day04
  (:require [clojure.string :as str]))
(use 'clojure.java.io)

(defn get-lines [fname]
  (with-open [r (reader fname)]
    (doall (line-seq r))))

(defn parse-int[x]
  (Integer/parseInt x))

(defn keys[]
  (for [x (range 5) y (range 5)] [x y]))

(defn build-board [numbers]

    (reduce #(assoc %1 %2 (nth (nth numbers (second %2)) (first %2))) {} (keys))


  )

(defn prep-data[lines]
  (let [boards (loop [[l & rest] (drop 2 lines)
                      output '()
                      current '()]
                 (if (nil? l)
                   (reverse (cons (reverse current ) output))
                   (if (= "" l)
                     (recur
                       rest
                       (cons (reverse current) output)
                       '())
                     (recur rest output (cons (map parse-int (filter #(not= "" %) (str/split l #"\s+"))) current))

                     )
                   ))
    nums (map parse-int (str/split (first lines) #","))]
    [nums (map build-board boards)]))



(defn mark-number[board number]
  (let [keysToMark (filter #(= (board %) number) (keys))]
    (reduce #(assoc %1 %2 nil) board keysToMark)))

(defn row-keys[row]
  (for [x (range 5)] [x row]))

(defn col-keys[col]
  (for [y (range 5)] [col y]))

(defn check-board[board]
  (or (some
        #(every? nil? (map board (row-keys %)))
        (range 5))
      (some
        #(every? nil? (map board (col-keys %)))
        (range 5))
      ))

(defn p1 []
  (let [[nums bs] (prep-data (get-lines "data/2021/d4"))
        [winning-board last-num] (loop [[n & rest] nums
                            boards bs]
                       (if n
                         (let [marked-boards (map #(mark-number % n) boards)
                               winning-board (first (filter check-board marked-boards))]
                           (if winning-board
                             [winning-board n]
                             (recur rest marked-boards)
                             )
                           )
                         "no winner found")

                       )]
    (* last-num (reduce + (filter #(not= nil %) (map winning-board (keys)))))

    ))


(defn p2 []
  (let [[nums bs] (prep-data (get-lines "data/2021/d4"))]
    (loop [[n & rest] nums
           boards bs
           last-winner nil]
      (println n)
      (if n
        (let [marked-boards (map #(mark-number % n) boards)
              winning-boards (filter check-board marked-boards)
              remaining-boards (filter #(not (check-board %)) marked-boards)]
          (if (= 1 (count winning-boards))
            (recur rest remaining-boards
                   (* n (reduce + (filter #(not= nil %) (map (first winning-boards) (keys))))))
            (recur rest remaining-boards last-winner)
            ))
        last-winner)

      )
    ))


(p1)

(p2)