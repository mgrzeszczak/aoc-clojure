(ns aoc-clojure.2021.day02
  (:require [clojure.string :as str]))
(use 'clojure.java.io)

(defn get-lines [fname]
  (with-open [r (reader fname)]
    (doall (line-seq r))))

(defn p1 []
  (let [lines (map
               (fn [x] (str/split x (re-pattern " ")))
               (get-lines "data/2021/d2"))]
    (loop [[curr & rem] lines
           p [0 0]]
      (if (nil? curr) (reduce * p)
                      (recur rem (map + p
                                      (case (first curr)
                                        "forward" [(Integer/parseInt (second curr)) 0]
                                        "up" [0 (- (Integer/parseInt (second curr)))]
                                        "down" [0 (Integer/parseInt (second curr))])

                                      )



                             )
                      )
      )
    ))

(defn p2 []
  (let [lines (map
                (fn [x] (str/split x (re-pattern " ")))
                (get-lines "data/2021/d2"))]
    (loop [[curr & rem] lines
           p [0 0 0]]
      (println curr p )
      (if (nil? curr) (* (first p) (second p))
                      (let [x (Integer/parseInt (second curr))
                            command (first curr)]
                        (recur rem (map + p
                                        (case command
                                          "forward" [x (* (nth p 2) x) 0]
                                          "up" [0 0 (- (Integer/parseInt (second curr)))]
                                          "down" [0 0 (Integer/parseInt (second curr))])

                                        )



                               )
                        )

                      )
      )
    ))

(p1)

(p2)
