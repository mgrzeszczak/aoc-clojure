(ns aoc-clojure.2021.day01)
(use 'clojure.java.io)

(defn get-lines [fname]
  (with-open [r (reader fname)]
    (doall (line-seq r))))

(defn p1 []
  (let [nums (map
               (fn [x] (Integer/parseInt x))
               (get-lines "data/2021/d1"))]
    ))

(defn p1-inner [nums]
  (count (filter #(> (second %) (first %)) (map vector (take (- (count nums) 1) nums) (drop 1 nums)))
         ))

(defn p2 []
  (let [nums (map
               (fn [x] (Integer/parseInt x))
               (get-lines "data/2021/d1"))
        size (count nums)
        a (take (- size 2) nums)
        b (take (- size 1) (drop 1 nums))
        c (drop 2 nums)
        sums (map #(reduce + %) (map vector a b c))]
    (p1-inner sums)))

(p1)
(p2)
