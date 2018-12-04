(ns aoc2018.util)

(defn tails [xs]
  (if (seq xs)
    (cons (seq xs)
          (lazy-seq (tails (rest xs))))
    '(())))

(defn find-first [p xs]
  (first (filter p xs)))
