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


(defn -main [& args]
  (println "hello moo! " (day3_2 (split-file "resources/day3.input"))))
