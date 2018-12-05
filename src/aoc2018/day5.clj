(ns aoc2018.day5
  (:require [clojure.string :as s]))

(def polymer (remove #{\newline} (slurp "resources/day5.txt")))
(defn annihilate? [x y]
  (and x
       (= (s/lower-case (str x))
          (s/lower-case (str y)))
       (not= x y)))

(defn react-unit [old-units new-unit]
  (if (annihilate? (first old-units)
                   new-unit)
    (rest old-units)
    (conj old-units
          new-unit)))

(defn react [polymer]
  (->> (reduce react-unit '() polymer)
       reverse))

;; (count (react polymer))
