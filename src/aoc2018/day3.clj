(ns aoc2018.day3
  (:require [clojure.set :refer [intersection]]
            [aoc2018.parser :refer :all]
            [aoc2018.util :refer :all]))

(def parse-entries
  (->> (let-parses [_ (parse-char \#)
                    id (parse+ parse-digit)
                    _ (parse-string " @ ")
                    [x, y] (parse-joined-by (parse-char \,)
                                            json-number)
                    _ (parse-string ": ")
                    [width, height] (parse-joined-by (parse-char \x)
                                                     json-number)]
         {:id (apply str id)
          :coords (for [x-coord (range x (+ x width))
                        y-coord (range y (+ y height))]
                    [x-coord y-coord])})
       parse-lines))

(def claimed-coords (result (run parse-entries (slurp "resources/day3.txt"))))

(defn overlapping-coords-set [claimed-coords]
  (loop [[claimed-coord & rest-claimed] claimed-coords
         encountered-coords #{}
         overlapping-coords #{}]
    (cond (nil? claimed-coord)
          overlapping-coords

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

;; Star 1
(defn area-of-overlapping-coords [claimed-coords]
  (count (overlapping-coords-set (mapcat :coords claimed-coords))))

;; (area-of-overlapping-coords claimed-coords)

;; Star 2
(defn non-overlapping-id [claimed-coords]
  (let [overlapping (overlapping-coords-set (mapcat :coords
                                                    claimed-coords))]
    (:id (find-first #(empty? (intersection overlapping
                                            (set (:coords %))))
                     claimed-coords))))

(non-overlapping-id claimed-coords)
