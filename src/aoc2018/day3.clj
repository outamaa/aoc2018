(ns aoc2018.day3
  (:require [aoc2018.parser :refer :all]
            [aoc2018.util :refer :all]))

(def parse-entries
  (->> (let-parses [_ (parse-seq (parse+ (parse-char-if #(not= % \@)))
                                 (parse-string "@ "))
                    [x, y] (parse-joined-by (parse-char \,)
                                            json-number)
                    _ (parse-string ": ")
                    [width, height] (parse-joined-by (parse-char \x)
                                                     json-number)]
         (for [x-coord (range x (+ x width))
               y-coord (range y (+ y height))]
           [x-coord y-coord]))
       parse-lines
       (interpret-as (partial mapcat identity))))

(def claimed-coords (result (run parse-entries (slurp "resources/day3.txt"))))

(defn area-of-overlapping-coords [claimed-coords]
  (loop [[claimed-coord & rest-claimed] claimed-coords
         encountered-coords #{}
         overlapping-coords #{}]
    (cond (nil? claimed-coord)
          (count overlapping-coords)

          (overlapping-coords claimed-coord)
          (recur rest-claimed encountered-coords overlapping-coords)

          (encountered-coords claimed-coord)
          (recur rest-claimed
                 ; This is only to reduce set size for future comparisons
                 (disj encountered-coords claimed-coord)
                 (conj overlapping-coords claimed-coord))

          :else
          (recur rest-claimed
                 (conj encountered-coords claimed-coord)
                 overlapping-coords))))

(area-of-overlapping-coords claimed-coords)
