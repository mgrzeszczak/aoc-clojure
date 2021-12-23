(ns aoc-clojure.2021.day23
  (:require [clojure.string :as str]
            [clojure.set :as sets]
            [aoc-clojure.lib.lib :as lib]))
(use 'clojure.java.io)
(require '[clojure.data.priority-map :refer [priority-map]])

(def allowed-chars (into #{} (map str "ABCD.")))

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (lib/parse-grid-2 (str/split-lines x))
        (filter #(allowed-chars (second %)) x)
        (into {} x)
        (let [grid x
              amphs (filter #(not= "." (second %)) x)
              room-coords (group-by first (sort-by first (map first amphs)))
              rooms (into {} (map-indexed (fn [i k] [(str (nth "ABCD" i)) (into #{} (room-coords k))]) (sort (keys room-coords))))
              illegal-for-all (into #{} (map (fn [[x y]] [x (dec y)]) (map #(apply min-key second %) (vals room-coords))))
              illegal-moves (into {} (map (fn [v] [v (reduce sets/union (conj (map second (filter #(not= v (first %)) rooms)) illegal-for-all))]) (map str "ABCD")))
              ]
          {
           :grid          grid
           :rooms         rooms
           :illegal-moves illegal-moves
           })))

(defn dijkstra
  [start neighbor-fn]
  (loop [queue (priority-map start 0)
         cost-map {start 0}]
    (if (empty? queue)
      cost-map
      (let [[current cost] (peek queue)]
        (let [ns (neighbor-fn current)
              to-update (filter (fn [[n d]] (< d (or (cost-map n) ##Inf))) (map (fn [[n d]] [n (+ d cost)]) ns))
              updated-cost-map (into cost-map to-update)
              new-queue (into (pop queue) to-update)]
          (recur new-queue updated-cost-map))))))

(defn neighbor-fn [grid]
  (fn [p]
    (map (fn [n] [n 1]) (filter #(= "." (grid %)) (lib/grid-neighbors-4 p)))))

(def move-cost-map {"A" 1 "B" 10 "C" 100 "D" 1000})

(defn move-cost [amph dist]
  (* (move-cost-map amph) dist))

(defn amphipods [grid]
  (filter (fn [[_ v]] (not= "." v)) grid))

(defn completed? [[grid _] rooms]
  (every? (fn [[p [a _]]] ((rooms a) p)) (amphipods grid)))

(defn create-state [grid]
  [(into {} (map (fn [[p v]] (if (= "." v) [p v] [p [v 0]])) grid)) 0])

(defn can-move? [[_ [_ cnt]]]
  (< cnt 2))

(defn room-can-be-entered[a room grid]
  (every?
    (fn[p]
      (let [v (grid p)]
        (or (= "." v) (= a (first v)))
        )
      )
    room)
  )

(defn next-states [[grid cost :as state] illegal-moves rooms]
  (let [nfs (neighbor-fn grid)
        amphs (amphipods grid)
        moveable (filter can-move? amphs)]
    (apply
      concat
      (map
        (fn [[p [a cnt]]]
          (let [reachable (filter (fn [[t _]] (and (not= p t) (not ((illegal-moves a) t)))) (dijkstra p nfs))
                legal (filter (fn [[t _]] (if (= 1 cnt) (and ((rooms a) t) (room-can-be-entered a (rooms a) grid)) true)) reachable)
                legal2 (filter (fn [[t _]] (if (= 0 cnt) (or (every? (fn[[_ r]] (not (r t))) rooms) (and ((rooms a) t) (room-can-be-entered a (rooms a) grid))) true)) legal)]
            (map (fn [[t c]]
                   [(into grid [[t [a (inc cnt)]] [p "."]]) (+ (move-cost a c) cost)]
                   ) legal2)
            )
          )
        moveable))))

(defn solve-2
  ([state rooms illegal-moves] (solve-2 state rooms illegal-moves ##Inf {} 0))
  ([[_ score :as state] rooms illegal-moves best cache depth]
   (if (< (rand-int 10000) 2) (println depth best))
   (if (cache state)
     [(cache state) cache]
     (if
       (or (completed? state rooms) (>= score best))
       [(min score best) cache]
       (let [to-eval (filter (fn[[_ score]] (< score best)) (next-states state illegal-moves rooms))]
         (reduce (fn [[b c] s]
                   (let [[nb nc] (solve-2 s rooms illegal-moves b c (inc depth))]
                     [(min nb b) nc]
                     )
                   ) [best cache] to-eval)
         ))
     )

   ))

(defn solve
  ([state rooms illegal-moves] (solve state rooms illegal-moves ##Inf 0))
  ([[_ score :as state] rooms illegal-moves best depth]
   (if (< (rand-int 10000) 2) (println depth best))
   (if
     (or (completed? state rooms) (>= score best))
     (min score best)
     (let [to-eval (filter (fn[[_ score]] (< score best)) (next-states state illegal-moves rooms))]
       (reduce (fn [b s]
                 (min (solve s rooms illegal-moves b (inc depth)) b)
                 ) best to-eval)
       ))))

(time (as-> (parse-input "data/2021/d23") x
            (let [state (create-state (:grid x))
                  rooms (:rooms x)
                  illegal-moves (:illegal-moves x)]
              ;(rooms "A")
              (solve-2 state rooms illegal-moves)
              ;(completed? state rooms)
              ;(next-states state illegal-moves)
              )))