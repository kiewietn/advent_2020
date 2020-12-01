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

(defn -main [& args]
  (println "hello moo! " (day1_2)))
