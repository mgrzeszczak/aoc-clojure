(ns aoc-clojure.2022.day09
  (:require [clojure.string :as str]
            [aoc-clojure.lib.lib :as lib]))
(use 'clojure.java.io)


(defn parse-line[line]
  (let [[dir v] (str/split line #" ")]
    [dir (Integer/parseInt v)]
    )
  )

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split-lines x)
        (map parse-line x)
        x))


(defn move[[hx hy] [tx ty] dir]
  (let [[nhx nhy] (case dir
                    "U" [hx (dec hy)]
                    "D" [hx (inc hy)]
                    "L" [(dec hx) hy]
                    "R" [(inc hx) hy])
        [dx dy] [(- nhx tx) (- nhy ty)]
        [xdir ydir] [(/ dx (max 1 (lib/abs dx))) (/ dy (max 1 (lib/abs dy)))]
        xok (<= (lib/abs dx) 1)
        yok (<= (lib/abs dy) 1)
        [ntx nty] (if
                    (and xok yok) [tx ty]
                    [(+ tx xdir) (+ ty ydir)]
                      )

        ]
    [[nhx nhy] [ntx nty]]
    ))

(defn follow[[hx hy] [tx ty]]
  (let [[dx dy] [(- hx tx) (- hy ty)]
        [xdir ydir] [(/ dx (max 1 (lib/abs dx))) (/ dy (max 1 (lib/abs dy)))]
        xok (<= (lib/abs dx) 1)
        yok (<= (lib/abs dy) 1)
        [ntx nty] (if
                    (and xok yok) [tx ty]
                                  [(+ tx xdir) (+ ty ydir)]
                                  )

        ]
    [ntx nty]
    )
  )

(defn mv[[x y] dir]
  (case dir
    "U" [x (dec y)]
    "D" [x (inc y)]
    "L" [(dec x) y]
    "R" [(inc x) y]
))

(defn solve-p1 []
  (as-> (parse-input "data/2022/d9") x
        (mapcat (fn [[dir v]] (map (fn[_] [dir 1]) (range v))) x)
        (loop [[curr & rem] x
               visited #{[0 0]}
               [hx hy] [0 0]
               [tx ty] [0 0]]
          (if (not curr)
            (count visited)
            (let [[dir _] curr
                  [nhx nhy] (mv [hx hy] dir)
                  [ntx nty] (follow [nhx nhy] [tx ty])]
              (recur rem (conj visited [ntx nty]) [nhx nhy] [ntx nty])
              )))))

(solve-p1)

(defn move-all [positions dir]
  (let [head (mv (positions 0) dir)]
    (loop [[curr & rem] (range 1 10)
           new-positions {0 head}]
      (if (not curr)
        new-positions
        (recur rem (conj new-positions [curr (follow (new-positions (dec curr)) (positions curr))]))

                     ))))

(defn solve-p2[]
  (as-> (parse-input "data/2022/d9") x
        (mapcat (fn [[dir v]] (map (fn[_] [dir 1]) (range v))) x)
        (loop [[curr & rem] x
               history #{[0 0]}
               positions (into {} (map #(vector % [0 0]) (range 10)))]
          (if (not curr)
            (count history)
            (let [[dir _] curr
                  new-positions (move-all positions dir)]
              (recur rem (conj history (new-positions 9)) new-positions)
              )))))

(solve-p2)