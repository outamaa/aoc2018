(ns aoc2018.day10
  (:require [aoc2018.parser :refer :all]
            [aoc2018.util :refer :all]))

(def serial-number 2694)

(defn cell-power [x y serial-number]
  (let [rack-id  (+ x 10)
        hundred (quot (* (+ (* y rack-id)
                            serial-number)
                         rack-id)
                      100)]
    (- (rem hundred 10)
       5)))

(defn grid [width height serial-number]
  (for [x (range width)]
    (for [y (range height)]
      (cell-power x y serial-number))))

(defn square-powers [grid sq-size]
  (->> grid
       (map (comp (partial map sum)
                  (partial partition sq-size 1)))
       transpose
       (mapv (comp (partial mapv sum)
                   (partial partition sq-size 1)))))

(defn max-coordinates [square-powers]
  ;; Note: transposed
  (let [height (count square-powers)
        width (count (first square-powers))]
    (loop [x 0
           y 0
           max-power Long/MIN_VALUE
           max-coords nil]
      (cond (>= y height)
            [max-power max-coords]

            (>= x width)
            (recur 0 (inc y) max-power max-coords)

            :else
            (let [coord-power (get (get square-powers y) x)
                  [new-max-power new-max-coords] (find-max first
                                                           [[coord-power [x y]]
                                                            [max-power max-coords]])]
              (recur (inc x) y new-max-power new-max-coords))))))

(defn max-cell-square-power [grid sq-size]
  (-> grid
      (square-powers sq-size)
      max-coordinates))

;;
;; Star 2
;;
;; Talk about naive implementation
(defn max-of-all-squares [grid]
  (->> (range 1 (inc (count (first grid))))
       (map (fn [sq-size]
              (conj (max-cell-square-power grid sq-size)
                    sq-size)))
       (find-max first)))
