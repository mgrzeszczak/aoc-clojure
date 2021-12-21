(ns aoc-clojure.2021.day19
  (:require [clojure.string :as str]
            [clojure.set :as sets]
            [aoc-clojure.lib.lib :as lib]))
(use 'clojure.java.io)
(require '[clojure.data.priority-map :refer [priority-map]])

(defn parse-coords [line]
  (vec (map (comp #(Integer/parseInt %) second) (re-seq #"(-?\d+)" line)))
  )

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split x #"\n\n")
        (map (comp #(map parse-coords %) #(drop 1 %) str/split-lines) x)
        ))

(defn relative-position [[x1 y1 z1 :as from] [x2 y2 z2 :as to]]
  (vec (map (fn [[a b]] (- a b)) [[x2 x1] [y2 y1] [z2 z1]])))

(defn relative-positions [beacon beacons]
  (into #{} (map #(relative-position beacon %) beacons)))

(defn positions-match? [positions-a positions-b]
  (let [count (count (filter positions-b positions-a))]
    (>= count 12)
    )
  )

(defn shuffle-fns []
  (list
    (fn [[a b c]] [a b c])
    (fn [[a b c]] [a c b])
    (fn [[a b c]] [b a c])
    (fn [[a b c]] [b c a])
    (fn [[a b c]] [c a b])
    (fn [[a b c]] [c b a])))

(defn rotation-fns []
  (for [x [-1 1]
        y [-1 1]
        z [-1 1]]
    (fn [[a b c]] [(* a x) (* b y) (* c z)])
    ))

(defn orientation-fns []
  (for [r (rotation-fns)
        s (shuffle-fns)]
    (comp s r)))

(defn orientations [v]
  (map (fn [f] (f v)) (orientation-fns)))

(defn match-beacons [beacon-a all-a beacon-b all-b]
  (let [relative-a (relative-positions beacon-a all-a)
        relative-b (relative-positions beacon-b all-b)]
    (first (filter identity (for [ofn (orientation-fns)]
                              (let [relative-b-oriented (into #{} (map ofn relative-b))]
                                (if (positions-match? relative-a relative-b-oriented)
                                  ofn
                                  nil
                                  )
                                )
                              )))
    )
  )

(defn relative-scanner-position [[x1 y1 z1] [x2 y2 z2]]
  [(- x1 x2) (- y1 y2) (- z1 z2)])


(defn try-match-scanners[scanner-a-beacons scanner-b-beacons]
  (let [result (first
                  (filter identity
                          (for [a scanner-a-beacons
                                b scanner-b-beacons]
                            (let [match (match-beacons a scanner-a-beacons
                                                       b scanner-b-beacons)]
                              (if match
                                [a b match]
                                nil
                                )))))]
    result
    )
  )

(defn has-not-nil?[coll]
  (not (empty? (filter identity coll))))


(defn add-pos[[x1 y1 z1] [x2 y2 z2]]
  [(+ x1 x2) (+ y1 y2) (+ z1 z2)])


(defn translate-beacons[beacons ofn relative-scanner-position]
  (into #{} (map #(add-pos relative-scanner-position (ofn %)) beacons)))

; works but long
(time (as-> (parse-input "data/2021/d19") x
            (let [scanners x
                  ;other-scanners (into {} (map (fn[s] [s (filter #(not= % s) scanners)]) scanners))
                  f (first scanners)]
              (loop [identified (into #{} f)
                     unidentified-scanners (rest scanners)]
                (println "count of identified: " (count identified))
                  (if (empty? unidentified-scanners)
                    (count identified)
                    (let [[beacons [a b ofn]] (first (filter #(not (nil? (second %))) (map #(vector % (try-match-scanners identified %)) unidentified-scanners)))
                          _ (println "found match" a b ofn)
                          rel-scanner-pos (relative-scanner-position a (ofn b))
                          translated (translate-beacons beacons ofn rel-scanner-pos)]
                      (recur (sets/union identified translated) (filter #(not= % beacons) unidentified-scanners))
                      )

                    )))))

(defn dist[[x1 y1 z1] [x2 y2 z2]]
  (reduce + (map (fn[[a b]] (lib/abs (- a b))) [[x1 x2] [y1 y2] [z1 z2]])))

(defn max-distance[positions]
  (reduce max (for [a positions
        b positions]
    (dist a b)
    ))
  )

(time (as-> (parse-input "data/2021/d19") x
            (let [scanners x
                  ;other-scanners (into {} (map (fn[s] [s (filter #(not= % s) scanners)]) scanners))
                  f (first scanners)]
              (loop [identified (into #{} f)
                     newly-identified identified
                     scanner-positions #{[0 0 0]}
                     unidentified-scanners (rest scanners)]
                (println "count of identified: " (count identified))
                (if (empty? unidentified-scanners)
                  [(count identified) (max-distance scanner-positions)]
                  (let [identified-scanners (filter #(not (nil? (second %))) (map #(vector % (try-match-scanners newly-identified %)) unidentified-scanners))
                        to-filter-out (into #{} (map first identified-scanners))
                        new-scanner-positions (into #{} (map (fn[[beacons [a b ofn]]]
                               (relative-scanner-position a (ofn b))
                               ) identified-scanners))
                        translated (reduce sets/union (map (fn[[beacons [a b ofn]]]
                               (let [_ (println "found match" a b ofn)
                                     rel-scanner-pos (relative-scanner-position a (ofn b))
                                     translated (translate-beacons beacons ofn rel-scanner-pos)]
                                 translated)
                               ) identified-scanners))
                        ]
                    (recur (sets/union identified translated) translated (sets/union scanner-positions new-scanner-positions) (filter #(not (to-filter-out %)) unidentified-scanners))
                    )

                  )))))
