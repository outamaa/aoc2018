(ns aoc2018.day13
  (:require [aoc2018.parser :refer :all]
            [aoc2018.util :refer :all]
            [clojure.string :as s]))

(def direction->coords
  {:north [-1 0]
   :south [1 0]
   :east [0 1]
   :west [0 -1]})

(defn add-coords [a b]
  (mapv + a b))

(defn neighbor-coords [coords neighbor]
  (mapv + coords
        (get direction->coords
             neighbor)))

(defn neighbors [c]
  (get {\- [:east :west]
        \> [:east :west]
        \< [:east :west]
        \| [:north :south]
        \^ [:north :south]
        \v [:north :south]
        \+ [:north :south :east :west]}
       c
       []))

(defn ->location [[coords c]]
  {:coords coords
   :c c
   :neighbors (set (map (partial neighbor-coords coords)
                        (neighbors c)))})

(defn add-neighbor [loc own-coords neighbor-loc]
  (update (or loc (->location [own-coords nil]))
          :neighbors #(conj (or % #{}) neighbor-loc)))

(defn add-neighbor-to-locations [locations new-neighbor neighbor-locations]
  (reduce (fn [locations neighbor-location]
            (update locations
                    neighbor-location
                    add-neighbor neighbor-location (:coords new-neighbor)))
          locations
          neighbor-locations))

(defn add-location [locations [coords c]]
  (let [location (->location [coords c])
        loc-neighbors (:neighbors location)]
    (-> locations
        (assoc coords location)
        (add-neighbor-to-locations location loc-neighbors))))

(defn heading [c]
  (get {\> :east
        \< :west
        \^ :north
        \v :south}
       c))

(defn ->elf [[coords c]]
  {:coords coords
   :heading (heading c)
   :next-turn :left})

(defn elf-location? [[_ c]]
  ((set "<>^v") c))

(defn add-elf [elves loc]
  (let [elf (->elf loc)]
    (conj elves elf)))

(def routes (for [[row line] (map-indexed vector (s/split-lines (slurp "resources/day13.txt")))
                  [col c] (map-indexed vector line) :when (not ((set "\\/ ") c))]
              [[row col] c]))

(def test-routes (for [[row line] (map-indexed vector (s/split-lines (slurp "resources/day13_test.txt")))
                       [col c] (map-indexed vector line) :when (not ((set "\\/ ") c))]
                   [[row col] c]))


(defn world [routes]
  {:locations (reduce add-location {} routes)
   :elves (reduce add-elf [] (filter elf-location? routes))})

(defn at-intersection? [elf world]
  (= 4 (count (:neighbors (get (:locations world)
                               (:coords elf))))))

(defn next-heading [heading turn]
  ({[:north :left] :west
    [:north :straight] :north
    [:north :right] :east
    [:east :left] :north
    [:east :straight] :east
    [:east :right] :south
    [:south :left] :east
    [:south :straight] :south
    [:south :right] :west
    [:west :left] :south
    [:west :straight] :west
    [:west :right] :north}
   [heading turn]))

(defn move-forward [elf world]
  (let [elf-location-neighbors (:neighbors (get (:locations world) (:coords elf)))]
    (cond (elf-location-neighbors (add-coords (:coords elf)
                                              (direction->coords (:heading elf))))
          (-> elf
              (update :coords add-coords (direction->coords (:heading elf))))

          (elf-location-neighbors (add-coords (:coords elf)
                                              (direction->coords (next-heading (:heading elf)
                                                                               :left))))
          (-> elf
              (update :coords add-coords (direction->coords (next-heading (:heading elf) :left)))
              (update :heading next-heading :left))

          (elf-location-neighbors (add-coords (:coords elf)
                                              (direction->coords (next-heading (:heading elf)
                                                                               :right))))
          (-> elf
              (update :coords add-coords (direction->coords (next-heading (:heading elf) :right)))
              (update :heading next-heading :right)))))

(defn turn [elf]
  (-> elf
      (update :heading next-heading (:next-turn elf))
      (update :next-turn #({:left     :straight
                            :straight :right
                            :right    :left}
                           %))))

(defn move [world elf]
  (if (at-intersection? elf world)
    (-> elf turn (move-forward world))
    (move-forward elf world)))

(defn tick [world]
  (assoc world :elves
         (->> (sort-by :coords (:elves world))
              (mapv (partial move world)))))

(defn collision-p [world]
  (let [[most-frequent-coord elves-in-coord] (->> (:elves world)
                                                  (map :coords)
                                                  max-frequency)]
    (when (> elves-in-coord 1)
      most-frequent-coord)))

(defn draw-char [world row col]
  (if-let [elf (find-first (comp #(= % [row col]) :coords)
                           (:elves world))]
    ({:north \^ :south \v :east \> :west \<} (:heading elf))
    (or (some-> world :locations (get [row col]) :c)
        \space)))

(defn draw-world [world]
  (let [[[row-min col-min] [row-max col-max]] (bounds (keys (:locations world)))]
    (doseq [row (range row-min (inc row-max))]
      (println (apply str (map (partial draw-char world row)
                               (range col-min (inc col-max))))))))

(defn find-collision [world]
  (loop [w world]
    ;; (draw-world w)
    ;; (Thread/sleep 500)
    (let [new-world (tick w)]
      (or (collision-p new-world)
          (recur new-world)))))

(defn tick [world]
  (assoc world :elves
         (->> (sort-by :coords (:elves world))
              (mapv (partial move world)))))

(defn tick-destroying-elves [world]
  (update world :elves
          (fn [elves]
            (loop [[elf & rest-elves] (sort-by :coords elves)
                   survivors []]
              (if (nil? elf)
                survivors
                (let [moved-elf (move world elf)]
                  (cond (find-first (comp (partial = (:coords moved-elf)) :coords)
                                    rest-elves)
                        (recur (->> rest-elves
                                    (remove (comp (partial = (:coords moved-elf)) :coords))
                                    (into []))
                               (->> survivors
                                    (remove (comp (partial = (:coords moved-elf)) :coords))
                                    (into [])))

                        (find-first (comp (partial = (:coords moved-elf)) :coords)
                                    survivors)
                        (recur rest-elves
                               (->> survivors
                                    (remove (comp (partial = (:coords moved-elf)) :coords))
                                    (into [])))

                        :else
                        (recur rest-elves
                               (conj survivors moved-elf)))))))))

(defn find-last-cart [world]
  (loop [w world]
    ;; (draw-world w)
    ;; (Thread/sleep 500)
    (if (= 1 (count (:elves w)))
      (:coords (first (:elves w)))
      (recur (tick-destroying-elves w)))))
