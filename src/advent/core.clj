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

(defn day6_1 []
  (let [raw-test-data (slurp "resources/day6.input")]
    (reduce + (map count (map #(into #{} %) (map #(clojure.string/replace % #"\n" "") (clojure.string/split raw-test-data #"\n\n")))))))

(defn day6_2 []
  (let [raw-test-data (slurp "resources/day6.input")]
    (reduce + (map count
                   (map #(apply set/intersection %)
                        (map (fn [group]
                               (map #(into #{} %) group)) (map #(clojure.string/split-lines %) (clojure.string/split raw-test-data #"\n\n"))))))))

(defn parse-bags [raw-input]
  (reduce into (map #(assoc {} (first %) (first (rest %)))
                    (map (fn [rule]
                           (let [[a-key the-values] rule]
                             (list (clojure.string/replace a-key #" bags " "")
                                   (map (fn [a-value]
                                          (clojure.string/trim (clojure.string/replace a-value #"bag?.*" ""))) the-values))))
                         (map (fn [a-vector]
                                (let [[a-key the-values] a-vector]
                                  (list a-key (clojure.string/split the-values #",")))) (map #(clojure.string/split % #"contain") raw-input))))))

(def test-line "light red bags contain 1 bright white bag, 2 muted yellow bags.")

;;got stuck, looked at solution from https://www.youtube.com/watch?v=uujzvDnEXp0
(defn parse-bag-entry [entry]
  (let [[bag & deps] (clojure.string/split entry #"\s?(contain|,)\s?")
        bag-color (re-find #"\w+ \w+" bag)]
    [bag-color (keep (comp next (partial re-find #"(\d+) (\w+ \w+)")) deps)]))

(defn bag-color-graph [entries]
  (reduce (fn [m [bag deps]]
            (reduce (fn [m [num color]]
                      (update m color conj bag)) m deps))
          {} entries))

(parse-bag-entry test-line)
(bag-color-graph (list (parse-bag-entry test-line)))

(defn add-valid [result graph color]
  (into result (get graph color)))

(defn valid-outermost [graph start]
  (loop [result (add-valid #{} graph start)]
    (let [result2 (reduce (fn [acc color]
                            (add-valid acc graph color)) result result)]
      (if (= result result2)
        result
        (recur result2)))))

(defn day7_1 []
  (count (valid-outermost (bag-color-graph (map parse-bag-entry (split-file "resources/day7.input"))) "shiny gold")))

;;;;;;;;; part 7.2

;;this outputs what I had with parse-bags fun
(defn nesting-bag-color-graph [entries]
  (reduce (fn [m [bag deps]]
            (reduce (fn [m [num color]]
                      (update m bag conj [(Long/parseLong num) color])) m deps))
          {} entries))

(def test-input-pt2 "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.")

(nesting-bag-color-graph (map parse-bag-entry (clojure.string/split-lines test-input-pt2)))

(defn color-count [graph color]
  (let [entries (get graph color)]
    (if (seq entries)
      (reduce (fn [acc [num color]]
                (+ acc (* num (color-count graph color))))
              1
              entries)
      1)))

(dec (color-count (nesting-bag-color-graph (map parse-bag-entry (clojure.string/split-lines test-input-pt2))) "shiny gold"))

(def day7-2 (dec (color-count (nesting-bag-color-graph (map parse-bag-entry (split-file "resources/day7.input"))) "shiny gold")))

(def day8-demo-input (clojure.string/split-lines "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"))

(defn parse-instruction [input]
  (let [[instruction argument] (clojure.string/split input #" ")]
    [instruction (Long/parseLong argument)]))

(defn get-next-index-for-instruction [instruction index]
  (cond (= "nop" (first instruction)) (inc index)
        (= "acc" (first instruction)) (inc index)
        (= "jmp" (first instruction)) (+ (nth instruction 1) index)))

(defn action-on-accumulator [acc instruction]
  (if (= "acc" (first instruction)) (+ acc (nth instruction 1))
      acc))

(defn cycle-catcher [instructions]
  (loop [visited-indeces #{}
         current-index 0
         acc 0]
    (if (contains? visited-indeces current-index)
      acc
      (let [instruction (parse-instruction (nth instructions current-index))
            next-index (get-next-index-for-instruction instruction current-index)]
        (recur (conj visited-indeces current-index) next-index (action-on-accumulator acc instruction))))))

(defn has-instruction-at-index? [instructions index]
  (< index (count instructions)))

(defn run-to-end [instructions]
  (loop [visited-indeces #{}
         current-index 0
         acc 0]
    (cond (contains? visited-indeces current-index)
      "cycle detected"
      (not (has-instruction-at-index? instructions current-index)) acc
      :else (let [instruction (nth instructions current-index)
            next-index (get-next-index-for-instruction instruction current-index)]
        (recur (conj visited-indeces current-index) next-index (action-on-accumulator acc instruction))))))

(defn toggle-instruction-at-index [instructions index]
  (let [instruction (nth instructions index)]
    (cond (= "nop" (first instruction)) (assoc instructions index ["jmp" (nth instruction 1)])
          (= "jmp" (first instruction)) (assoc instructions index ["nop" (nth instruction 1)])
          :else instructions)))

(defn day8-1 [instructions]
  (cycle-catcher instructions))

;;from cycle-catcher function for initial input
(def visited-indeces #{0 558 453 586 291 443 70 62 580 430 370 110 311 377 213 472 7 473 466 454 205 459 175 322 1 24 490 55 568 206 39 345 4 550 204 77 197 405 518 119 319 293 329 144 504 505 176 54 307 517 137 234 15 242 251 585 437 516 159 429 309 458 31 136 139 460 581 174 363 284 305 514 214 442 561 235 304 40 467 445 317 294 364 515 412 308 56 500 168 347 501 237 292 143 247 474 551 376 316 303 560 310 567 238 196 162 461 541 243 29 348 539 28 608 538 411 64 465 334 323 198 155 295 587 285 590 489 436 588 17 3 536 332 330 544 2 236 373 142 359 371 444 537 566 215 277 19 609 452 431 9 457 145 244 245 378 446 404 283 138 346 333 53 559 78 562 542 315 203 321 320 133 81 79 173 421 582 160 30 10 499 270 543 271 18 403 52 209 161 372 406 71 579 80 591 37 63 212 362 8})

(defn flip-instruction-test [instructions]
  (reduce (fn [acc index]
            (conj acc (run-to-end (toggle-instruction-at-index instructions index))))
          '()
          visited-indeces))

(defn day8-2 []
  (filter #(not (= "cycle detected" %)) (flip-instruction-test (vec (map parse-instruction (split-file "resources/day8.input"))))))

(def day9-test-input (map #(Long/parseLong %) (clojure.string/split-lines "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576")))

(defn sliding-window [coll window]
  (loop [acc []
         remaining coll]
    (if (or (> window (count remaining)) (empty? remaining))
      acc
      (recur (conj acc (take window remaining)) (drop 1 remaining)))))

(defn find-outlier [input pre-amble-size]
  (reduce (fn [acc window]
            (if (empty? (let [under-test (last window)
                              pre-amble (take pre-amble-size window)
                              diff (into #{} (map #(- under-test %) pre-amble))]
                          (clojure.set/intersection (into #{} pre-amble) diff)))
              (conj acc (last window))
              acc))
          '() (sliding-window input (inc pre-amble-size))))

(defn day9-1 []
  (find-outlier (map #(Long/parseLong %) (split-file "resources/day9.input")) 25))


(defn max-sequence [target input]
  (let [sums (for [window-size (take (dec (count input)) (reverse (range (count input))))
                   :let [sum (filter #(= target (apply + %)) (sliding-window input window-size))]
                   :when (not (empty? sum))]
               sum)]
    (:seq (apply max-key :size (map #(let [element (first %)
                                      size (count element)]
                                  {:size size :seq element}) sums)))))

(defn day9-2 [target input]
  (let [max-seqs (max-sequence target input)
        the-min (apply min max-seqs)
        the-max (apply max max-seqs)]
    (+ the-min the-max)))

(defn day10-1 []
  (let [sorted-input (sort (map #(Long/parseLong %) (split-file "resources/day10.input")))]
    (frequencies (cons 3 (cons (first sorted-input) (map #(- %2 %1) sorted-input (rest sorted-input)))))))

(def day11-input (split-file "resources/day11.input"))

(def day11-test-input (clojure.string/split-lines "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"))

;;x is columns
;;y is rows

(def coord-map (let [input day11-input]
                 (reduce (fn [acc entry]
                           (assoc acc (first entry) (first (rest entry))))
                         {}
                         (for [k (range (count (first input)))
                               r (range (count input))
                               :let [row (nth input r)]]
                           (list (list r k) (nth row k))))))

(defn get-elements-around-point [point coord-map]
  (filter #(not (= nil %))
          (let [current-x (first point)
                current-y (first (rest point))]
            (for [x (range (dec current-x) (+ 2 current-x))
                  y (range (dec current-y) (+ 2 current-y))
                  :when (not (and (= current-x x) (= current-y y)))]
              (get coord-map (list x y))))))

(defn get-next-state-for-point [point coord-map]
  (let [element-at-point (get coord-map point)
        surroundings (get-elements-around-point point coord-map)]
    (cond (and (= \L element-at-point) (not-any? #(= \# %) surroundings)) \#
          (and (= \# element-at-point) (> (count (filter #(= \# %) surroundings)) 3)) \L
          :else element-at-point)))

(defn get-max-rows [coord-map]
  (apply max (map #(first %) (keys coord-map))))

(defn get-max-cols [coord-map]
  (apply max (map #(first (rest %)) (keys coord-map))))

(defn get-coordinates-N-of-point [point coord-map]
  (for [offset (range (inc (first point)))
        :when (not (= 0 offset))]
    (list (- (first point) offset) (first (rest point)))))

(defn get-coordinates-S-of-point [point coord-map]
  (for [offset (range (first point) (inc (get-max-rows coord-map)))
        :when (not (= (first point) offset))]
    (list offset (first (rest point)))))

(defn get-coordinates-E-of-point [point coord-map]
  (for [offset (range (first (rest point)) (inc (get-max-cols coord-map)))
        :when (not (= (first (rest point)) offset))]
    (list (first point) offset)))

(defn get-coordinates-W-of-point [point coord-map]
  (for [offset (range (inc (first (rest point))))
        :when (not (= (first (rest point)) offset))]
    (list (first point) offset)))

(defn get-coordinates-NW-of-point [point coord-map]
  (for [offset (range (inc (first point)))
        :when (not (or (= 0 offset) (< (- (first (rest point)) offset) 0)))]
    (list (- (first point) offset) (- (first (rest point)) offset))))

(defn get-coordinates-NE-of-point [point coord-map]
  (for [offset (range (inc (first point)))
        :when (not (or (= 0 offset) (> (+ (first (rest point)) offset) (get-max-cols coord-map))))]
    (list (- (first point) offset) (+ (first (rest point)) offset))))

(defn get-coordinates-SE-of-point [point coord-map]
  (for [offset (range (first point) (inc (get-max-rows coord-map)))
        :when (not (or (= (first point) offset) (> (+ (first (rest point)) (- offset (first point))) (get-max-cols coord-map))))]
    (list offset (+ (first (rest point)) (- offset (first point))))))

(defn get-coordinates-SW-of-point [point coord-map]
  (for [offset (range (first point) (inc (get-max-rows coord-map)))
        :when (not (or (= (first point) offset) (< (- (first (rest point)) (- offset (first point))) 0)))]
    (list offset (- (first (rest point)) (- offset (first point))))))

(defn get-entries-in-all-directions [point coord-map]
  (map #(select-keys coord-map %) (map #(% point coord-map) (list get-coordinates-N-of-point
                                                                get-coordinates-E-of-point
                                                                get-coordinates-S-of-point
                                                                get-coordinates-W-of-point
                                                                get-coordinates-NW-of-point
                                                                get-coordinates-NE-of-point
                                                                get-coordinates-SE-of-point
                                                                get-coordinates-SW-of-point))))

(defn distance-between-points [point1 point2]
  (let [delta-x (- (first point2) (first point1))
        delta-y (- (first (rest point2)) (first (rest point1)))]
    (+ (* delta-x delta-x) (* delta-y delta-y))))

(defn get-elements-surrounding-los [point coord-map]
  (filter #(not (= nil %))
          (map #(first (rest %)) (map first (map (fn [elements]
                                                   (sort
                                                    #(compare (distance-between-points point (key %1))
                                                              (distance-between-points point (key %2))) elements))
                                                 (map #(filter (fn [entry]
                                                                 (not (= \. (val entry)))) %)
                                                      (get-entries-in-all-directions point coord-map)))))))

(defn day11-1 []
  (count
   (filter #(= \# %)
           (vals (loop [seating-plan coord-map]
                   (let [next-iter
                         (reduce-kv (fn [m k v]
                                      (assoc m k (get-next-state-for-point k seating-plan)))
                                    {}
                                    seating-plan)]
                     (if (= next-iter seating-plan)
                       next-iter
                       (recur next-iter))))))))

(defn get-next-state-for-point-part-2 [point coord-map]
  (let [element-at-point (get coord-map point)
        surroundings (get-elements-surrounding-los point coord-map)]
    (cond (and (= \L element-at-point) (not-any? #(= \# %) surroundings)) \#
          (and (= \# element-at-point) (> (count (filter #(= \# %) surroundings)) 4)) \L
          :else element-at-point)))

(defn day11-2 []
  (count
   (filter #(= \# %)
           (vals (loop [seating-plan coord-map]
                   (let [next-iter
                         (reduce-kv (fn [m k v]
                                      (assoc m k (get-next-state-for-point-part-2 k seating-plan)))
                                    {}
                                    seating-plan)]
                     (if (= next-iter seating-plan)
                       next-iter
                       (recur next-iter))))))))
