(ns aoc2018.day11
  (:require [aoc2018.parser :refer :all]
            [aoc2018.util :refer :all]))

(def parse-particle
  (let-parses [_ (parse-string "position=<")
               pos (parse-joined-by (parse-string ", ")
                                    (skip-whitespace json-number))
               _ (parse-string "> velocity=<")
               vel (parse-joined-by (parse-string ", ")
                                    (skip-whitespace json-number))
               _ (parse-string ">")]
    {:pos pos
     :vel vel}))

(def parse-particles (parse-lines parse-particle))

(def particles (result (run parse-particles (slurp "resources/day10.txt"))))

(defn integrate [particle]
  (update particle
          :pos
          #(mapv + % (:vel particle))))

(defn integrate-system [particles]
  (map integrate particles))

(defn system-bounds [particles]
  (bounds (map :pos particles)))

(defn system-size [particles]
  (->> (reverse (system-bounds particles))
       (apply map -)
       (mapv abs)))

(defn system-char [pos-set x y]
  (if (pos-set [x y])
    \#
    \.))

(defn print-system [system n]
  (let [[[x0 y0] [x1 y1]] (system-bounds system)
        [width height] (system-size system)
        pos-set (set (map :pos system))]
    (println "Iteration" n)
    (dotimes [y (inc height)]
      (println (apply str (mapv #(system-char pos-set (+ % x0) (+ y y0))
                                (range width)))))
    (println "")))

(loop [n 0
       size (system-size particles)
       system particles]
  (let [new-size (system-size system)]
    (cond (= n 10521) ;; Found by testing, had to do some debugging
                      ;; involving off-by-one errors. :D
          (print-system system n)

          :else
          (recur (inc n) new-size (integrate-system system)))))
