(ns aoc2018.util)

(defn tails [xs]
  (if (seq xs)
    (cons (seq xs)
          (lazy-seq (tails (rest xs))))
    '(())))

(defn find-first [p xs]
  (first (filter p xs)))

(defn max-frequency
  "Returns kv-pair where k is the most frequent item and v is the frequency"
  [xs]
  (->> xs
       frequencies
       (apply max-key second)))
