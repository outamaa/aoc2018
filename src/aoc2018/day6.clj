(ns aoc2018.day6
  (:require [aoc2018.parser :refer :all]
            [aoc2018.util :refer :all]
            [clojure.set :as set]))

(def parse-location
  (parse-joined-by (parse-string ", ")
                   json-number))

(def parse-locations
  (parse-lines parse-location))

(def x-coord first)
(def y-coord second)
(defn loc [x y]
  [x y])

(def locations (result (run parse-locations (slurp "resources/day6.txt"))))

(defn manhattan-distance [a b]
  (->> (map - a b)
       (map abs)
       (reduce +)))

(defn borders-of-infinity [locations]
  (bounds locations))

(defn within? [[upper-left lower-right] [x y]]
  (and (<= (x-coord upper-left) x)
       (<= (y-coord upper-left) y)
       (>= (x-coord lower-right) x)
       (>= (y-coord lower-right) y)))

(defn in-infinite-region? [locations]
  (let [borders (borders-of-infinity locations)]
    (fn [location]
      (not (within? borders location)))))

(defn nearest-start-location [locations]
  (fn [location]
    (let [distances (->> locations
                         (map #(vector % (manhattan-distance location %))))
          nearest (find-min second distances)]
      (if (= 1 (count (filter #(= (second %) (second nearest))
                              distances)))
        (first nearest)
        nil))))

(defn neighbors [[x y]]
  #{[x (inc y)]
    [x (dec y)]
    [(inc x) y]
    [(dec x) y]})

(defn valid-neighbors [start-locations-set infinite?]
  (fn [location]
    (->> (neighbors location)
         (remove start-locations-set)
         set)))

(defn new-neighbors-to-check [neighbors-fn checked location]
  (set/difference (neighbors-fn location) checked))

(defn region [start-locations neighbors-fn infinite? accept?]
  (fn [location]
    (loop [locations-to-check (neighbors-fn location)
           region #{location}
           checked #{}]
      (let [location-to-check (first locations-to-check)]
        (cond  (nil? location-to-check)
               region

               (and (accept? location-to-check location)
                    (infinite? location-to-check))
               :infinite

               (accept? location-to-check location)
               (recur (set/union (disj locations-to-check location-to-check)
                                 (new-neighbors-to-check neighbors-fn
                                                         checked
                                                         location-to-check))
                      (conj region location-to-check)
                      (conj checked location-to-check))

               :else
               (recur (disj locations-to-check location-to-check)
                      region
                      (conj checked location-to-check)))))))


(defn regions [start-locations]
  (let [infinite? (in-infinite-region? start-locations)
        neighbors (valid-neighbors (set start-locations) infinite?)
        nearest   (nearest-start-location start-locations)]
    (map (juxt identity
               (region start-locations
                       neighbors
                       infinite?
                       #(= (nearest %1) %2)))
         start-locations)))

;; (->> (regions locations) (map second) (remove #(= :infinite %)) (map count) find-max)

;;
;; Star 2
;;

(defn center-coords [start-locations]
  (->> start-locations
       transpose
       (mapv (comp long avg))))

(defn distance-sum [start-locations location]
  (->> start-locations
       (map (partial manhattan-distance location))
       (reduce +)))

(defn acceptable-distance-sum? [start-locations]
  (fn [location _]
    (< (distance-sum start-locations location) 10000)))

(defn safe-region [start-locations]
  (let [infinite? (in-infinite-region? start-locations)
        accept?   (acceptable-distance-sum? start-locations)]
    ((region start-locations
             neighbors
             infinite?
             accept?)
     (center-coords start-locations))))
