(ns aoc2018.day5
  (:require [aoc2018.parser :refer :all]
            [aoc2018.util :refer :all]
            [clojure.string :as s]))

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

;;
;; Star 2
;;
(defn remove-component-type [polymer component]
  (remove (set (str (s/lower-case component)
                    (s/upper-case component)))
          polymer))

(defn minimum-reaction [polymer]
  (->> (char-range \a \z)
       (map (partial remove-component-type polymer))
       (map react)
       (map count)
       (find-max -)))
