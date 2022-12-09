(ns aoc-clojure.2022.day07
  (:require [clojure.string :as str]))
(use 'clojure.java.io)

(defn parse-line [line]
  (let [cd (re-matches #"\$ cd (.+)" line)
        ls (re-matches #"\$ ls" line)
        file (re-matches #"(\d+) ([\w\.]+)" line)
        dir (re-matches #"dir ([\w\.]+)" line)
        ]
    (cond
      cd [:cd (second cd)]
      ls [:ls]
      file (let [[_ size name] file]
             [:file (Integer/parseInt size) name])
      dir [:dir (second dir)]
      )
    )
  )

(defn parse-input [file-name]
  (as-> (slurp file-name) x
        (str/split-lines x)
        (map parse-line x)
        x))


(defn parse-commands [commands]
        (loop [[curr & rem] commands
               dir []
               contents {}]
          (if (not curr)
            contents
            (let [cmd (first curr)
                  path (str/join "/" dir)]
              (case cmd
                :cd (recur rem (if (= ".." (second curr)) (vec (drop-last 1 dir)) (conj dir (second curr))) contents)
                :ls (recur rem dir (if (contents path) contents (assoc contents path {:file-size 0 :folders []})))
                :file (let [dir-contents (contents path)]
                        (recur rem dir (assoc contents path (assoc dir-contents :file-size (+ (dir-contents :file-size) (second curr)))))
                        )
                :dir (let [dir-contents (contents path)]
                       (recur rem dir (assoc contents path (assoc dir-contents :folders (conj (dir-contents :folders) (second curr)))))
                       ))))))

(defn folder-size [folder contents]
  (let [{file-size :file-size
         folders   :folders} (contents folder)]
    (+ file-size (reduce + (map #(folder-size (str folder "/" %) contents) folders)))

    )
  )

(def disk-size 70000000)
(def required-space 30000000)

(defn solve []
  (let [contents (parse-commands (parse-input "data/2022/d7"))
        used-space (folder-size "/" contents)
        remaining-space (- disk-size used-space)
        needed-space (- required-space remaining-space)
        dir-sizes (map #(folder-size % contents) (keys contents))
        ]
    [
     (reduce + (filter #(<= % 100000) dir-sizes))
     (apply min (filter #(>= % needed-space) dir-sizes))
     ]
    ))

(solve)
