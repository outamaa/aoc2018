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

(defn peek-pop [q]
  [(first q)
   (rest q)])

(defn apply-n [n f]
  (apply comp (repeat n f)))

;;
;; Deque
;;

(def empty-deque
  {:top '()
   :bottom '()})

(defn insert-top [deque x]
  (update deque :top conj x))

(defn insert-bottom [deque x]
  (update deque :bottom conj x))

(defn empty-deque? [deque]
  (and (empty? (:top deque))
       (empty? (:bottom deque))))

(defn- rearrange-deque [deque]
  {:top (reverse (:bottom deque))
   :bottom (reverse (:top deque))})

(defn- pop-deque [deque end]
  (cond (empty-deque? deque) nil
        (empty? (end deque)) (recur (rearrange-deque deque) end)
        :else
        [(first (end deque))
         (update deque end rest)]))

(defn pop-top [deque]
  (pop-deque deque :top))

(defn pop-bottom [deque]
  (pop-deque deque :bottom))

(defn- peek-deque [deque end]
  (cond (empty-deque? deque) nil
        (empty?  (end deque)) (recur (rearrange-deque deque) end)
        :else
        (first (end deque))))

(defn peek-top [deque]
  (peek-deque deque :top))

(defn peek-bottom [deque]
  (peek-deque deque :bottom))

;;
;; 2d vec
;;
(defn vec2d [x y] [x y])
(def x-coord first)
(def y-coord second)

(defn bounds [locations]
  "Finds bounding coordinates for a seq of 2d vectors"
  [[(x-coord (find-min x-coord locations))
    (y-coord (find-min y-coord locations))]
   [(x-coord (find-max x-coord locations))
    (y-coord (find-max y-coord locations))]])

(defn within-bounds?
  [[upper-left lower-right] [x y]]
  (and (<= (x-coord upper-left) x)
       (<= (y-coord upper-left) y)
       (>= (x-coord lower-right) x)
       (>= (y-coord lower-right) y)))
