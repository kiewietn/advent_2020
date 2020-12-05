(ns advent.core
  (:require [clojure.set :as set]))

(defn split-file [file]
  (clojure.string/split-lines (slurp file)))

(defn day1_1 []
  (first (let [vals (map #(Integer. %) (split-file "resources/day1.input"))]
     (for [x vals
           y vals
           :when (= 2020 (+ x y))] (* x y)))))

(defn day1_2 []
  (let [vals (map #(Integer. %) (split-file "resources/day1.input"))]
    (for [x vals
          y vals
          z vals
          :when (= 2020 (+ x y z))] (* x y z))))

(defn parse-key-range [key-range]
  (let [[valid-range value] (clojure.string/split key-range #" ")
        [lower-bound upper-bound] (clojure.string/split valid-range #"-")]
    [[(Integer. lower-bound) (Integer. upper-bound)] value]))

(defn parse-password [composed-password]
  (let [[key-range key-val]
        (map #(clojure.string/trim %1) (clojure.string/split composed-password #":"))
        parsed-key-range (parse-key-range key-range)]
    (list parsed-key-range key-val)))

(defn valid-password? [parsed-password]
  (let [lower-bound (nth (first (first parsed-password)) 0)
        upper-bound (nth (first (first parsed-password)) 1)
        test-char (first (first (rest (first parsed-password))))
        password (first (rest parsed-password))
        character-frequency (count (filter #(= test-char %) password))]
    (and (<= character-frequency upper-bound)
         (>= character-frequency lower-bound))))

(defn valid-password2? [parsed-password]
  (let [lower-index (dec (nth (first (first parsed-password)) 0))
        upper-index (dec (nth (first (first parsed-password)) 1))
        test-char (first (first (rest (first parsed-password))))
        password (vec (first (rest parsed-password)))]
    (and (or (= test-char (nth password lower-index))
         (= test-char (nth password upper-index)))
         (not (and (= test-char (nth password upper-index))
                   (= test-char (nth password lower-index)))))))

(defn day2_1 [password-combos]
  (let [parsed-passwords (map #(parse-password %) password-combos)
        valid-passwords (filter #(valid-password? %) parsed-passwords)]
    (count valid-passwords)))

(defn day2_2 [password-combos]
  (let [parsed-passwords (map #(parse-password %) password-combos)
        valid-passwords (filter #(valid-password2? %) parsed-passwords)]
    (count valid-passwords)))

(defn day3_1 [test-data row-step col-step]
  (loop [num-trees 0
         row 0
         col 0]
    (if (empty? (get test-data row))
      num-trees
      (let [tree-row (get test-data row)
            is-tree? (= \# (nth tree-row col))
            trees-found (if is-tree? (inc num-trees) num-trees)]
        (recur trees-found (+ row row-step) (mod (+ col col-step) (count tree-row)))))))

(defn day3_2 [test-data]
  (reduce * (map #(day3_1 test-data (first %1) (first (rest %1))) '((1 1) (1 3) (1 5) (1 7) (2 1)))))

(defn parse-passports [raw-data]
        (let [passport-strings (map #(clojure.string/replace % #"\n" " ") (clojure.string/split raw-data #"\n\n"))]
          (map #(reduce (fn [a-map key-val]
                          (let [[a-key a-val] (clojure.string/split key-val #":")]
                            (assoc a-map a-key a-val))) {} %) (map #(clojure.string/split % #" ") passport-strings))))

(defn day4_1 [raw-data]
  (count (filter #(every? (dissoc % "cid") '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid")) (parse-passports raw-data))))

(defn valid-birth-year? [birth-year]
        (cond (= 4 (count birth-year))
              (let [as-number (Integer. birth-year)]
                (and (>= as-number 1920) (<= as-number 2002)))
              :else false))

(defn valid-issue-year? [issue-year]
        (cond (= 4 (count issue-year))
              (let [as-number (Integer. issue-year)]
                (and (>= as-number 2010) (<= as-number 2020)))
              :else false))

(defn valid-expiration-year? [expiration-year]
        (cond (= 4 (count expiration-year))
              (let [as-number (Integer. expiration-year)]
                (and (>= as-number 2020) (<= as-number 2030)))
              :else false))

(defn valid-height? [height]
        (cond (clojure.string/ends-with? height "cm")
              (let [as-number (Integer. (clojure.string/replace height "cm" ""))]
                (and (>= as-number 150) (<= as-number 193)))
              (clojure.string/ends-with? height "in")
              (let [as-number (Integer. (clojure.string/replace height "in" ""))]
                (and (>= as-number 59) (<= as-number 76)))
              :else false))

(defn valid-hcl? [color]
  (re-matches #"^#[a-z0-9]{6}" color))

(defn valid-eye-color? [color]
  (some #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} (list color)))

(defn valid-pid? [pid]
  (re-matches #"[0-9]{9}" pid))

(defn day4_2 [raw-data]
  (let [passport-candidates (filter #(every? (dissoc % "cid") '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid")) (parse-passports raw-data))]
    (count (filter #(and (valid-birth-year? (get % "byr"))
                   (valid-expiration-year? (get % "eyr"))
                   (valid-eye-color? (get % "ecl"))
                   (valid-hcl? (get % "hcl"))
                   (valid-height? (get % "hgt"))
                   (valid-issue-year? (get % "iyr"))
                   (valid-pid? (get % "pid"))) passport-candidates))))

(defn get-row-code [code]
  (subs code 0 7))

(defn get-col-code [code]
  (subs code 7))

(defn get-range-for-code [code the-range lower-half-char upper-half-char]
  (let [lower-bound (first the-range)
        upper-bound (first (rest the-range))]
    (cond (= lower-half-char code) (list lower-bound (dec (+ lower-bound (/ (- (inc upper-bound) lower-bound) 2))))
          (= upper-half-char code) (list (+ lower-bound (/ (- (inc upper-bound) lower-bound) 2)) upper-bound))))

(defn get-row [row-code]
  (first
   (loop [codes row-code
          the-range '(0 127)]
     (if (empty? codes)
       the-range
       (recur (rest codes) (get-range-for-code (first codes) the-range \F \B))))))

(defn get-column [col-code]
  (first
   (loop [codes col-code
          the-range '(0 7)]
     (if (empty? codes)
       the-range
       (recur (rest codes) (get-range-for-code (first codes) the-range \L \R))))))

(defn get-seat-id [boarding-pass]
  (let [row (get-row (get-row-code boarding-pass))
        column (get-column (get-col-code boarding-pass))]
    (list row column (+ column (* 8 row)))))

(defn day5_1 []
  (let [seat-ids (map #(get-seat-id %) (split-file "resources/day5.input"))]
    (apply max (map #(first (rest (rest %))) seat-ids))))

(defn get-seat-coord [boarding-pass]
  (let [row (get-row (get-row-code boarding-pass))
        column (get-column (get-col-code boarding-pass))]
    (list row column)))

(defn day5_2 []
  (let [seat-ids (map #(get-seat-id %) (split-file "resources/day5.input"))]
    (filter (fn [seat-id]
              (let [seat-id-val (first (rest (rest seat-id)))
                    row (first seat-id)
                    col (first (rest seat-id))]
                (or (= 713 seat-id-val) (= 715 seat-id-val)))) seat-ids)))

(defn -main [& args]
  (println "hello moo! " (day5_2)))
