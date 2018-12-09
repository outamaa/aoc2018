(ns aoc2018.day9
  (:require [aoc2018.parser :refer :all]
            [aoc2018.util :refer :all]))

(def n-players 478)
(def n-marbles 71240)

;;
;; Circle
;;
(defn add-marble [circle marble]
  (insert-top circle marble))

(defn circle [marble]
  (add-marble empty-deque marble))

(defn rotate-cw [circle]
  (let [[x new-circle] (pop-bottom circle)]
    (insert-top new-circle x)))

(defn rotate-ccw [circle]
  (let [[x new-circle] (pop-top circle)]
    (insert-bottom new-circle x)))

(defn current-marble [circle]
  (peek-top circle))

(defn remove-current-marble [circle]
  (pop-top circle))


(defn insert-marble [circle marble]
  (if (zero? (rem marble 23))
    (let [[removed-marble new-circle] (-> circle
                                          ((apply-n 7 rotate-ccw))
                                          remove-current-marble)]
      [(+ marble removed-marble) (rotate-cw new-circle)])
    [0 (-> circle rotate-cw (add-marble marble))]))

(defn next-player [player n-players]
  (inc (rem player n-players)))

(defn scores [n-players n-marbles]
  (loop [scores (zipmap (map inc (range n-players))
                        (repeat 0))
         c (circle 0)
         player 1
         marble 1]
    (if (> marble n-marbles)
          scores
	  (let [[score new-circle] (insert-marble c marble)]
            (recur (update scores player + score)
                   new-circle
                   (next-player player n-players)
                   (inc marble))))))
