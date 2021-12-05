(ns aoc-clojure.2020.day20
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def N 10)

(defn parse-row [index content]
  (map-indexed #(vector [%1 index] %2) (str/split content #"")))

(defn edge [data which]
  (let [parse-fn (fn [dt] (Integer/parseInt (str/replace (str/replace (apply str dt) "#" "1") "." "0") 2))]
    (case which
      :left (parse-fn (map data (for [y (range 0 N)] [0 y])))
      :right (parse-fn (map data (for [y (range 0 N)] [(- N 1) y])))
      :top (parse-fn (map data (for [x (range 0 N)] [x 0])))
      :bottom (parse-fn (map data (for [x (range 0 N)] [x (- N 1)])))
      )))

(defn parse-tile-data [data]
  (reduce concat (map-indexed parse-row data)))

(defn create-tile [no data]
  (let [data-map (into {} data)]
    {
     :no    no
     :sides (into {} (map #(vector % (edge data-map %)) [:left :right :top :bottom]))
     :data  data-map
     }))

(defn parse-tile [[header & raw-data]]
  (let [parsed-data (parse-tile-data raw-data)
        no (Integer/parseInt (second (re-matches #"Tile (\d+):" header)))]
    (create-tile no parsed-data)))

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split x #"\n\n")
        (map str/split-lines x)
        (map parse-tile x)))

(defn flip-x
  ([tile] (flip-x tile N))
  ([tile N] (let [new-data (map (fn [[key val]] (vector [(- (- N 1) (first key)) (second key)] val)) (:data tile))]
              (create-tile (:no tile) new-data))))

(defn rotate-cw
  ([tile] (rotate-cw tile N))
  ([tile N] (let [new-data (map (fn [[key val]] (vector [(- (- N 1) (second key)) (first key)] val)) (:data tile))]
              (create-tile (:no tile) new-data))))

(defn generate-orientations
  ([tile] (generate-orientations tile N))
  ([tile N] (loop [t1 tile
                   t2 (flip-x tile N)
                   result '()
                   iter 0]
              (if
                (= iter 4)
                result
                (recur (rotate-cw t1 N) (rotate-cw t2 N) (conj (conj result t1) t2) (inc iter))))))

(defn opposite-side [side]
  (case side
    :left :right
    :right :left
    :top :bottom
    :bottom :top))

(defn match-tile [current side tiles]
  (let [val (side (:sides current))
        other-side (opposite-side side)]
    (first (filter #(= (other-side (:sides %)) val) (apply concat (map generate-orientations tiles))))
    )
  )

(defn solve-puzzle [tiles]
  (let [tile-nos (into #{} (map #(:no %) tiles))
        tiles-by-no (into {} (map #(vector (:no %) %) tiles))]
    (loop [queue (list (first tiles))
           mappings {}
           oriented-tiles (list (first tiles))]
      (if (empty? queue)
        (list (into {} (map #(vector (:no %) %) oriented-tiles)) mappings)
        (let [[current & rest] queue
              queue-nos (into #{} (map #(:no %) queue))
              remaining (map tiles-by-no (set/difference tile-nos queue-nos))
              current-mappings (into #{} (filter #(mappings [% (:no current)]) [:left :right :top :bottom]))
              missing-sides (set/difference #{:left :right :top :bottom} current-mappings)
              found-matches (filter #(not (nil? (second %))) (map #(vector % (match-tile current % remaining)) missing-sides))
              new-mappings (reduce #(assoc %1 (first %2) (second %2)) mappings (reduce concat (map #(vector [[(first %) (:no current)] (:no (second %))] [[(opposite-side (first %)) (:no (second %))] (:no current)]) found-matches)))
              new-oriented-tiles (concat oriented-tiles (map second found-matches))
              new-queue (concat rest (map second found-matches))]
          (recur new-queue new-mappings new-oriented-tiles))))))

(defn visualize [tile]
  (println (str/join "\n"
                     (map (fn [x] (apply str (map (fn [y] ((:data tile) [y x])) (range 0 N)))) (range 0 N)))))

(defn find-corners [mappings]
  (let [ids (distinct (map second (keys mappings)))]
    (filter (fn [id] (= 2 (count (filter #(= (second %) id) mappings)))) ids)))

; part 1
(time (as-> (parse-input "data/input_d20") x
            (solve-puzzle x)
            (reduce * (find-corners (second x)))))

; part 2

(defn find-top-left-corner [mappings]
  (let [ids (distinct (vals mappings))]
    (first (filter #(and (nil? (mappings [:left %])) (nil? (mappings [:top %]))) ids))))

(defn extract-tile-row [tile row]
  (apply str (map #((:data tile) [% row]) (range 1 (- N 1)))))

(defn connect-image-row [row tiles-by-no]
  (let [tiles (map tiles-by-no row)
        row-nums (range 1 (- N 1))]
    (map (fn [row] (apply str (map #(extract-tile-row % row) tiles))) row-nums)))

(defn connect-image-rows [rows]
  (reduce concat rows))

(defn index-image [image]
  (let [data (into {} (reduce concat (map-indexed (fn [i row] (map-indexed (fn [j char] [[i j] char]) row)) image)))
        n (first (apply max-key #(first %) (keys data)))]
    {
     :data data
     :n    (inc n)
     }
    )

  )

(defn make-image [[tiles mappings]]
  (let [ordering (let [corner (find-top-left-corner mappings)]
                   (loop [current-row [corner]
                          current-cell corner
                          dir :right
                          rows []]
                     (let [neighbor (mappings [dir current-cell])]
                       (if (nil? neighbor)
                         (let [bottom-neighbor (mappings [:bottom current-cell])]
                           (let [complete-row (if (= dir :left) (vec (reverse current-row)) current-row)]
                             (if (nil? bottom-neighbor)
                               (conj rows complete-row)
                               (recur [bottom-neighbor] bottom-neighbor (opposite-side dir) (conj rows complete-row))
                               )))
                         (recur (conj current-row neighbor) neighbor dir rows)))))]
    (let [image (connect-image-rows (map #(connect-image-row % tiles) ordering))]
      (index-image (map #(str/split % #"") image)))

    ))

(defn load-monster-pattern [file-name]
  (as-> (slurp file-name) x
        (str/split-lines x)
        (map #(str/split % #"") x)
        (map first (filter #(= (second %) "#") (apply concat (map-indexed (fn [i r] (map-indexed (fn [j c] [[i j] c]) r)) x))))
        (let [[f & rest] x]
          (map #(vector (- (first %) (first f)) (- (second %) (second f))) rest))))

(defn apply-offset [pos offset]
  [(+ (first pos) (first offset)) (+ (second pos) (second offset))])

(defn find-monsters [data n]
  (let [pattern (load-monster-pattern "data/input_d20_monster")]
    (let [all-positions (reduce concat (map
                                         (fn [i] (map
                                                   (fn [j] [i j])
                                                   (range 0 n)))
                                         (range 0 n)))
          hash-positions (filter #(= "#" (data %)) all-positions)]
      (count (filter (fn [pos] (every? #(= % "#") (map data (map #(apply-offset pos %) pattern)))) hash-positions)))))

(count (filter #(= "#" %) (str/split (slurp "data/input_d20") #"")))

(time (as-> (parse-input "data/input_d20") x
            (solve-puzzle x)
            (make-image x)
            (- (count (filter #(= % "#") (vals (:data x)))) (* 15 (reduce max (map #(find-monsters (:data %) (:n x)) (generate-orientations x (:n x))))))))