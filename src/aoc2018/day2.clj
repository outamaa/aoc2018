(ns aoc2018.day2
  (:require [aoc2018.parser :refer :all]
            [aoc2018.util :refer :all]))

(def parse-ids
  (parse-lines (parse+ parse-lowercase-char)))

(def id-seqs (result (run parse-ids (slurp "resources/day2.txt"))))

;;
;; Part 1
;;

(defn checksum [ids]
  (->> ids
       (map (comp distinct
                  (partial filterv #{2 3})
                  vals
                  frequencies))
       flatten
       frequencies
       vals
       (apply *)))

;; Answer: (checksum id-seqs)

;;
;; Part 2
;;

(defn differ-by-one-char?
  ([a b]
   (boolean (when (= (count a) (count b))
              (differ-by-one-char? a b false))))
  ([a b found-pair?]
   (loop [[ac & a-rest] a
          [bc & b-rest] b
          found?        found-pair?]
     (cond (nil? ac)
           found?

           (and (not= ac bc) found?) ; Found another pair, the princess is not in this castle
           false

           (not= ac bc)
           (recur a-rest b-rest true)

           :else
           (recur a-rest b-rest found?)))))

(defn common-chars-string [a b]
  (->> (map vector a b)
       (filter (partial apply =))
       (map first)
       (apply str)))

(defn find-the-box [ids]
  (loop [[id & rest-ids]                        ids
         [ids-to-compare & rest-ids-to-compare] (tails (rest ids))]
    (if-let [differing-by-one (find-first (partial differ-by-one-char? id)
                                          ids-to-compare)]
      (common-chars-string id differing-by-one)
      (recur rest-ids rest-ids-to-compare))))

(find-the-box id-seqs)
