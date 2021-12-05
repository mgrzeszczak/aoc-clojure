(ns aoc-clojure.2020.day04
  (:require [clojure.set :as set])
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(defn str2char [s] (first (char-array s)))

(defn with-file [fname fn]
  (with-open [r (io/reader fname)]
    (do (fn (line-seq r)))))

(defn fields [line]
  (map #(drop 1 %) (re-seq #"(\w+):([^\s]+)" (if line line ""))))

(defn split-passports-r [[l & lines] current all]
  (if (not l)
    (if (empty? current) all (cons (str/join " " current) all))
    (if (= "" l)
      (recur lines '() (cons (str/join " " current) all))
      (recur lines (cons l current) all))))

(defn split-passports [lines] (split-passports-r lines '() '()))

; part 1
(defn valid-passport-p1 [p]
  (let [required (set '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))]
    (empty? (set/difference required p))))

(with-file "data/2020/input_d4"
           #(->> (do (split-passports %))
                 (map fields)
                 (map (fn [x] (set (map first x))))
                 (filter valid-passport-p1)
                 (count)))

; part 2
(defn parse-int [v]
  (try
    (Integer/parseInt v)
    (catch NumberFormatException e nil)))

(defn validate-byr [byr]
  (let [x (parse-int byr)]
    (if (nil? x) false (and (>= x 1920) (<= x 2002)))))
(defn validate-iyr [iyr]
  (let [x (parse-int iyr)]
    (if (nil? x) false (and (>= x 2010) (<= x 2020)))))
(defn validate-eyr [eyr]
  (let [x (parse-int eyr)]
    (if (nil? x) false (and (>= x 2020) (<= x 2030)))))
(defn validate-hgt [hgt]
  (let [[hs unit] (drop 1 (re-matches #"(\d+)(cm|in)" hgt)) h (parse-int hs)]
    (case unit
      "cm" (and (>= h 150) (<= h 193))
      "in" (and (>= h 59) (<= h 76))
      false)))
(defn validate-hcl [hcl]
  (not (nil? (re-matches #"#[a-f0-9]{6}" hcl))))
(defn validate-ecl [ecl]
  (not (nil? ((set '("amb" "blu" "brn" "gry" "grn" "hzl" "oth")) ecl))))
(defn validate-pid [pid]
  (not (nil? (re-matches #"\d{9}" pid))))

(defn valid-passport-p2-r [[[key val] & rest]]
  (if (not key)
    true
    (and
      (case key
        "byr" (validate-byr val)
        "iyr" (validate-iyr val)
        "eyr" (validate-eyr val)
        "hgt" (validate-hgt val)
        "hcl" (validate-hcl val)
        "ecl" (validate-ecl val)
        "pid" (validate-pid val)
        true)
      (recur rest))))

(defn valid-passport-p2 [p]
  (and (valid-passport-p1 (set (map first p)))
       (valid-passport-p2-r p)))

(with-file "data/2020/input_d4"
           #(->> (do (split-passports %))
                 (map fields)
                 (filter valid-passport-p2)
                 (count)))

