(ns aoc-clojure.2022.day24
  (:require [aoc-clojure.lib.lib :as lib]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.data.priority-map :refer [priority-map]]
            ))

(use 'clojure.java.io)

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (lib/parse-grid-2 (str/split-lines x))
        (into {} (filter #(not= "#" (second %)) x))
        )
  )

(defn add-vec [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn dir-dx [dir]
  (case dir
    ">" [1 0]
    "<" [-1 0]
    "^" [0 -1]
    "v" [0 1]
    )
  )

(defn move-blizzard [[pos dir] [min-x max-x min-y max-y]]
  (let [[nx ny] (add-vec pos (dir-dx dir))]
    (cond
      (> nx max-x) [min-x ny]
      (< nx min-x) [max-x ny]
      (> ny max-y) [nx min-y]
      (< ny min-y) [nx max-y]
      :else [nx ny]
      )))

(defn move-blizzards [blizzards boundary]
  (let [res (map (fn [[pos dir]]
                   [(move-blizzard [pos dir] boundary) dir]
                   ) blizzards)]
    res
    )

  )

(defn lcm [a b]
  (let [a (biginteger a)
        b (biginteger b)]
    (.divide (.multiply a b) (.gcd a b))
    )
  )


(defn generate-blizzard-fn [grid]
  (let [blizzards (filter #(#{"<" ">" "^" "v"} (second %)) grid)
        min-x (apply min (map first (keys grid)))
        max-x (apply max (map first (keys grid)))
        min-y (apply min (map second (keys grid)))
        max-y (apply max (map second (keys grid)))
        boundary [min-x max-x (inc min-y) (dec max-y)]
        width (inc (- max-x min-x))
        height (- (inc (- max-y min-y)) 2)
        period (lcm width height)
        [blizzard-states _] (reduce
                              (fn [[result blizzards] time]
                                (let [next-blizzards (move-blizzards blizzards boundary)]
                                  [(into result [[time next-blizzards]]) next-blizzards]
                                  )
                                )
                              [{0 blizzards} blizzards]
                              (range 1 period))
        blizzards-by-time (into {} (map (fn [[time state]] [time (into #{} (map first state))]) blizzard-states))
        ]
    (fn [position time]
      ((blizzards-by-time (mod time period)) position))

    )
  )

(defn create-moves-fn [grid blizz-fn]
  (fn [pos time]
    (let [all (conj (lib/grid-neighbors-4 pos) pos)]
      (filter (fn [[x y]]
                (and
                  (not (blizz-fn [x y] time))
                  (#{"." "<" ">" "^" "v"} (grid [x y]))
                  (#{"." "<" ">" "^" "v"} (grid pos))
                  )
                ) all)
      )
    )
  )

(defn fastest-path [moves-fn start end start-time]
  (loop [queue [[start start-time]]
         seen #{[start start-time]}]
    (if (empty? queue)
      (throw "empty queue")
      (let [[pos time] (queue 0)]
        (if (= pos end)
          time
          (let [possible-next-states (map #(vector % (inc time)) (moves-fn pos (inc time)))
                new-next-states (filter #(not (seen %)) possible-next-states)]
            (recur
              (into
                (subvec queue 1)
                new-next-states
                )
              (into seen new-next-states)
              )
            )
          )
        )
      )
    )
  )


(defn solve []
  (let [grid (parse-input "data/2022/d24")
        min-x (apply min (map first (keys grid)))
        max-x (apply max (map first (keys grid)))
        min-y (apply min (map second (keys grid)))
        max-y (apply max (map second (keys grid)))
        start [min-x min-y]
        end [max-x max-y]
        blizz-fn (generate-blizzard-fn grid)
        moves-fn (create-moves-fn grid blizz-fn)
        p2-1 (fastest-path moves-fn start end 0)
        p2-2 (fastest-path moves-fn end start p2-1)
        p2-3 (fastest-path moves-fn start end p2-2)
        ]
    [
     p2-1
     p2-3
     ]

    )
  )

(time (solve))