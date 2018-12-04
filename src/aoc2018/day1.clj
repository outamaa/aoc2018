(ns aoc2018.day1
  (:require [aoc2018.parser :refer :all]))

(def parse-frequencies
  (parse-lines json-number))

(def frequency-changes (result (run parse-frequencies (slurp "resources/day1.txt"))))

;;
;; Star 1
;;
(def resulting-frequency (reduce + 0 frequency-changes))

;;
;; Star 2
;;
(def frequencies (reductions + 0 (cycle frequency-changes)))

(def first-repeating-frequency
  (loop [[f & f-rest]            frequencies
         encountered-frequencies #{}]
    (if (encountered-frequencies f)
      f
      (recur f-rest
             (conj encountered-frequencies f)))))
