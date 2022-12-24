(ns aoc-clojure.2022.day22
  (:require [aoc-clojure.lib.lib :as lib]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            ))
(use 'clojure.java.io)


(def tile-size 50)

(defn turn [facing dir]
  (mod (+ facing (case dir "L" -1 "R" 1)) 4))

(defn step [facing]
  (case facing
    ; right
    0 [1 0]
    ; down
    1 [0 1]
    ; left
    2 [-1 0]
    ; up
    3 [0 -1]
    ))

(defn add-vec [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn precompute-wraparounds [grid]
  (let [y-wraparound (into {} (map (fn [[y coords]] [y [(apply min (map first coords)) (apply max (map first coords))]]) (group-by second (keys grid))))
        x-wraparound (into {} (map (fn [[y coords]] [y [(apply min (map second coords)) (apply max (map second coords))]]) (group-by first (keys grid))))]
    [y-wraparound x-wraparound]))

(defn wraparound [[x y] facing y-wraparound x-wraparound]
  (case facing
    ; right
    0 [(first (y-wraparound y)) y]
    ; down
    1 [x (first (x-wraparound x))]
    ; left
    2 [(second (y-wraparound y)) y]
    ; up
    3 [x (second (x-wraparound x))]
    )
  )

(defn create-move-fn-p1 [grid]
  (let [[y-wraparound x-wraparound] (precompute-wraparounds grid)]
    (fn [current facing]
      (let [p (add-vec current (step facing))
            target (if (not (grid p)) (wraparound p facing y-wraparound x-wraparound) p)
            ]
        (if (not= "#" (grid target)) target nil)
        ))))

(defn wraparound-cube [current facing tile-fn tile2pos-fn]
  ;(println "calling WRAPAROUND")
  (let [tile (tile-fn current)
        [x y] current
        [rel-x rel-y] [(mod x tile-size) (mod y tile-size)]]
    ;(println tile [x y] [rel-x rel-y])
    ;(println "wraparound from:" current "at tile:" tile "rel-pos:" [rel-x rel-y])
    (case tile
      1 (case facing
          ; right
          0 [(add-vec current (step facing)) facing]
          ; down
          1 [(add-vec current (step facing)) facing]
          ; left
          2 [(add-vec (tile2pos-fn 4) [0 (dec (- tile-size rel-y))]) 0]
          ; up
          3 [(add-vec (tile2pos-fn 6) [0 rel-x]) 0]
          )
      2 (case facing
          ; right
          0 [(add-vec (tile2pos-fn 5) [(dec tile-size) (dec (- tile-size rel-y))]) 2]
          ; down
          1 [(add-vec (tile2pos-fn 3) [(dec tile-size) rel-x]) 2]
          ; left
          2 [(add-vec current (step facing)) facing]
          ; up
          3 [(add-vec (tile2pos-fn 6) [rel-x (dec tile-size)]) facing]
          )
      3 (case facing
          ; right
          0 [(add-vec (tile2pos-fn 2) [rel-y (dec tile-size)]) 3]
          ; down
          1 [(add-vec current (step facing)) facing]
          ; left
          2 [(add-vec (tile2pos-fn 4) [rel-y 0]) 1]
          ; up
          3 [(add-vec current (step facing)) facing]
          )
      4 (case facing
          ; right
          0 [(add-vec current (step facing)) facing]
          ; down
          1 [(add-vec current (step facing)) facing]
          ; left
          2 [(add-vec (tile2pos-fn 1) [0 (dec (- tile-size rel-y))]) 0]
          ; up
          3 [(add-vec (tile2pos-fn 3) [0 rel-x]) 0]
          )
      5 (case facing
          ; right
          0 [(add-vec (tile2pos-fn 2) [(dec tile-size) (dec (- tile-size rel-y))]) 2]
          ; down
          1 [(add-vec (tile2pos-fn 6) [(dec tile-size) rel-x]) 2]
          ; left
          2 [(add-vec current (step facing)) facing]
          ; up
          3 [(add-vec current (step facing)) facing]
          )
      6 (case facing
          ; right
          0 [(add-vec (tile2pos-fn 5) [rel-y (dec tile-size)]) 3]
          ; down
          1 [(add-vec (tile2pos-fn 2) [rel-x 0]) facing]
          ; left
          2 [(add-vec (tile2pos-fn 1) [rel-y 0]) 1]
          ; up
          3 [(add-vec current (step facing)) facing]
          )
      )
    )
  )

(defn create-move-fn-p2 [grid pos2tile-fn tile2pos-fn]
  (fn [current facing]
    (let [p (add-vec current (step facing))
          [px py] p]
      ;(println)
      ;(println "current" current "facing" facing )
      ;(println "after simple move" p)
      ;(println "tile test" [(pos2tile-fn current) (pos2tile-fn p)])
      ;(println)
      (let [[target new-facing] (if

                                  (or
                                    (not= (pos2tile-fn current) (pos2tile-fn p))
                                    (< (min px py) 0)
                                    (>= px 150)
                                    (>= py 200)
                                    )

                                  ;tile changed
                                  (wraparound-cube current facing pos2tile-fn tile2pos-fn)
                                  ;same tile
                                  [p facing])]
        ;(println "testing" target "from" p)
        (if (not= "#" (grid target)) [target new-facing] nil))
      )


      ))

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (let [[grid-data path-data] (str/split x #"\n\n")]
          [
           (into {} (filter #(#{"#" "."} (second %)) (lib/parse-grid-2 (str/split-lines grid-data))))
           (map #(if (re-matches #"R|L" %) [:turn %] [:forward (Integer/parseInt %)]) (re-seq #"\d+|R|L" path-data))
           ]
          )
        ))

(defn find-start-pos [grid]
  (let [start-y (apply min (map second (keys grid)))
        start-x (apply min (map first (filter #(= start-y (second %)) (keys grid))))]
    [start-x start-y]
    )
  )

(defn move-forward [pos facing steps move-fn]
  (loop [c pos
         step 0
         tiles #{pos}]
    (if (= step steps)
      [c tiles]
      (let [next (move-fn c facing)]
        ;(println next)
        (if (not next)
          [c tiles]
          (recur next (inc step) (conj tiles next))
          )
        )
      )

    )
  )

(defn move-forward-v2 [pos init-facing steps move-fn]
  ;(println ["moving forward" steps "steps from" pos "facing" init-facing])
  (loop [c pos
         step 0
         facing init-facing
         tiles #{pos}]
    ;(println "stepping in" facing "from" c)
    (if (< (apply min c) 0) (throw "error negative coord"))
    (if (= step steps)
      [[c facing] tiles]
      (let [res (move-fn c facing)]
        (if (not res)
          [[c facing] tiles]
          (let [[next new-facing] res]
            (recur
              next
              (inc step)
              new-facing
              (conj tiles next))
            )

          )
        )
      )

    )
  )

(defn password [[x y] facing]
  (+ (* 1000 (inc y)) (* 4 (inc x)) facing)
  )

(defn follow-path [start-pos path move-fn]
  (loop [pos start-pos
         facing 0
         [instr & rem] path
         road #{start-pos}]
    (if (not instr)
      (do
        ;(lib/print-grid (into grid (map #(vector % "x") road)))
        (password pos facing)
        )
      (let [[type x] instr]
        (if (= type :turn)
          (recur pos (turn facing x) rem road)
          (let [[new-pos road-tiles] (move-forward pos facing x move-fn)]
            (recur new-pos facing rem (into road road-tiles))
            )
          )
        )
      )
    ))

(defn follow-path-v2 [start-pos path move-fn]
  (loop [pos start-pos
         facing 0
         [instr & rem] path
         road #{start-pos}]
    ;(println ["at" pos "facing" facing])
    (if (not instr)
      (do
        ;(lib/print-grid (into grid (map #(vector % "x") road)))
        (password pos facing)
        )
      (let [[type x] instr]
        ;(println "next instr" [type x])
        (if (= type :turn)
          (recur pos (turn facing x) rem road)
          (let [[[new-pos new-facing] road-tiles] (move-forward-v2 pos facing x move-fn)]
            ;(println "after-move-forward at" new-pos "facing" new-facing)
            (recur new-pos new-facing rem (into road road-tiles))
            )
          )
        )
      )
    ))



(defn create-tile-fns [grid]
  (loop [tile-no 0
         found-tiles 0
         matched {}
         tile-to-ref {}]
    (if (= tile-no 12)
      [(fn [[x y]]
         (matched [(quot x tile-size) (quot y tile-size)])
         )
       tile-to-ref
       ]

      (let [[x y] [(* (mod tile-no 3) tile-size) (* (quot tile-no 3) tile-size)]]
        (if (grid [x y])
          (recur (inc tile-no) (inc found-tiles) (assoc matched [(mod tile-no 3) (quot tile-no 3)] (inc found-tiles))
                 (assoc tile-to-ref (inc found-tiles) [(* (mod tile-no 3) tile-size) (* (quot tile-no 3) tile-size)]))
          (recur (inc tile-no) found-tiles matched tile-to-ref)
          )
        )
      )
    )
  )

(defn solve []
  (let [[grid path] (parse-input "data/2022/d22")
        start-pos (find-start-pos grid)
        move-fn-p1 (create-move-fn-p1 grid)
        [pos2tile-fn tile2pos-fn] (create-tile-fns grid)
        move-fn-p2 (create-move-fn-p2 grid pos2tile-fn tile2pos-fn)
        ]
    [(follow-path start-pos path move-fn-p1)
    (follow-path-v2 start-pos path move-fn-p2)]
    )
  )

(time (solve))