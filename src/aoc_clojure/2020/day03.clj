(ns aoc-clojure.2020.day03)
(use 'clojure.java.io)

(defn str2char [s] (first (char-array s)))
(defn with-file [fname fn]
  (with-open [r (reader fname)]
    (do (fn (line-seq r)))))


(defn is-tree [grid x y]
  (= \# (get (nth grid y) (mod x (count (first grid))))) )

(defn traverse [grid x y dx dy trees]
  (if (>= y (count grid))
    trees (recur grid (+ x dx) (+ y dy) dx dy (+ trees (if (is-tree grid x y) 1 0)))))
; part 1

(with-file "data/2020/input_d03"
           (fn [lines]
             (let [grid (doall lines)]
               (traverse grid 0 0 3 1 0))))

; part 2
(with-file "data/2020/input_d03"
           (fn [lines]
             (let [grid (doall lines)]
               (->> (list '(1 1) '(3 1) '(5 1) '(7 1) '(1 2))
                    (map #(traverse grid 0 0 (first %) (second %) 0))
                    (reduce *)))))