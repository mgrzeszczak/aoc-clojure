(ns aoc-clojure.2022.day16
  (:require [aoc-clojure.lib.lib :as lib]
            [clojure.string :as str]))
(use 'clojure.java.io)

(defn parse-line [line]
  (let [[_ valve flow-rate valves] (first (re-seq #"Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.+)" line))]
    [valve {:flow-rate (Integer/parseInt flow-rate) :valves (str/split valves #", ")}]))

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split-lines x)
        (let [parsed-lines (map parse-line x)]
          [(ffirst parsed-lines) (into {} parsed-lines)]
          )))

(defn neighbors [valve data]
  (map #(vector % 1) (:valves (data valve)))
  )

(defn find-distances [valve data]
  [valve (lib/dijkstra valve #(neighbors % data))]
  )

(defn p1-r
  ([current remaining distance-map flow-rate-map]
   (p1-r current remaining 0 30 distance-map flow-rate-map)
   )
  ([current remaining score time distance-map flow-rate-map]
   (if (empty? remaining)
     score
     (apply
       max
       (map (fn [v]
              (let [new-time (- time (+ 1 ((distance-map current) v)))]
                (p1-r v (disj remaining v) (+ score (* (flow-rate-map v) new-time)) new-time distance-map flow-rate-map)
                )
              ) remaining)
       ))))


(defn p1-r2
  [self current remaining time distance-map flow-rate-map]
  (let [valve-score (* (flow-rate-map current) time)]
    (if (empty? remaining)
      valve-score
      (apply
        max
        (map (fn [v]
               (let [new-time (- time (+ 1 ((distance-map current) v)))]
                 (+ valve-score (self self v (disj remaining v) new-time distance-map flow-rate-map))
                 )
               ) remaining)
        ))
    )
  )

(defn make-fn-p1 []
  (let [fn (memoize p1-r2)]
    (partial fn fn)
    )
  )


(defn solve []
  (let [[start-valve data] (parse-input "data/2022/d16")
        distance-map (into {} (map #(find-distances % data) (keys data)))
        flow-rate-map (into {} (map #(vector % (:flow-rate (data %))) (keys data)))
        working-valves (into #{} (filter #(> (flow-rate-map %) 0) (keys data)))
        ]
    ((make-fn-p1) start-valve working-valves 30 distance-map flow-rate-map)
    )
  )

;; this day was solved in python instead, solution in clojure to be added

(time (solve))