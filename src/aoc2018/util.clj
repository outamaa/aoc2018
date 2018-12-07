(ns aoc2018.util)

(defn tails [xs]
  (if (seq xs)
    (cons (seq xs)
          (lazy-seq (tails (rest xs))))
    '(())))

(defn find-first [p xs]
  (first (filter p xs)))

(defn find-max
  ([xs]
   (find-max identity xs))
  ([f xs]
   (apply max-key f xs)))

(defn find-min
  ([xs]
   (find-max - xs))
  ([f xs]
   (apply max-key (comp - f) xs)))


(defn max-frequency
  "Returns kv-pair where k is the most frequent item and v is the frequency"
  [xs]
  (->> xs
       frequencies
       (apply max-key second)))

(defn abs [n]
  (max n (- n)))

(defn transpose [xs]
  (apply mapv vector xs))

(defn avg [xs]
  (/ (reduce + xs)
     (count xs)))
