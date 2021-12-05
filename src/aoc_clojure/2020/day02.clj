(ns aoc-clojure.2020.day02)
(use 'clojure.java.io)

(defn with-file [fname fn]
  (with-open [r (reader fname)]
    (do (fn (line-seq r)))))

; part 1
(with-file "data/2020/input_d02"
           (fn [lines]
             (->> (do lines)
                  (map #(re-matches #"(\d+)-(\d+) (\w): (\w+)" %))
                  (map #(list (Integer/parseInt (nth % 1))
                              (Integer/parseInt (nth % 2))
                              ((frequencies (nth % 4)) (first (char-array (nth % 3))))))
                  (filter #(and (nth % 2) (>= (nth % 2) (first %)) (<= (nth % 2) (second %))))
                  (count))))


; part 2
(defn str2char [s] (first (char-array s)))
(with-file "data/2020/input_d02"
           (fn [lines]
             (->> (do lines)
                  (map #(re-matches #"(\d+)-(\d+) (\w): (\w+)" %))
                  (map #(list (dec (Integer/parseInt (nth % 1)))
                              (dec (Integer/parseInt (nth % 2)))
                              (str2char (nth % 3))
                              (nth % 4)))
                  (filter #(or
                             (and (= (nth % 2) (get (nth % 3) (nth % 0))) (not (= (nth % 2) (get (nth % 3) (nth % 1)))))
                             (and (= (nth % 2) (get (nth % 3) (nth % 1))) (not (= (nth % 2) (get (nth % 3) (nth % 0)))))
                             ))
                  (count))))