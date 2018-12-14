(ns aoc2018.day12
  (:require [aoc2018.parser :refer :all]
            [aoc2018.util :refer :all]))

(def parse-pot
  (interpret-as (comp boolean #{\#})
                (parse-char-if (set ".#"))))

(def parse-rule
  (let-parses [pots (parse+ parse-pot)
               _ (parse-string " => ")
               resulting-pot parse-pot]
    (when resulting-pot
      (into [] pots))))

(def parse-initial-state
  (let-parses [_ (parse-string "initial state: ")
               pots (parse+ parse-pot)
               _ (parse* parse-whitespace-char)
               rules (parse-lines parse-rule)]
    {:pots (->> pots
                (map-indexed (fn [idx pot?] (when pot? idx)))
                (remove nil?)
                set)
     :rules (->> rules
                 (remove nil?)
                 set)}))

(defn rule-tree [rules]
  (if (seq (first rules))
    (map-vals (comp rule-tree
                    (partial map rest))
              (group-by first rules))
    true))

#_(defn match-rule [rule-tree rule]
  (get-in rule-tree rule))
(defn match-rule [rules rule]
  (boolean (rules rule)))

(def pots-and-rules (result (run parse-initial-state (slurp "resources/day12.txt"))))
(def pots (:pots pots-and-rules))
#_(def rules (rule-tree (:rules pots-and-rules)))
(def rules (:rules pots-and-rules))

(defn neighbors [pot pots]
  (->> (range (- pot 2)
              (+ pot 3))
       (map (comp boolean pots))))

(defn next-generation [rules pots pot]
  (when (match-rule rules (neighbors pot pots))
    pot))

(defn draw [pots]
  (println (apply str (map #(if (pots %) \# \.) (range -15 100)))))

(defn pot-generations [pots rules]
  (iterate (fn [[gen pots]]
             (let [min-pot (find-min pots)
                   max-pot (find-max pots)
                   pot-range (range (dec min-pot)
                                    (+ max-pot 2))
                   next-gen (->> pot-range
                                 (map (partial next-generation rules pots))
                                 (remove nil?)
                                 (into #{}))]
               (do (println (inc gen) min-pot max-pot (sum next-gen))
                   [(inc gen) next-gen])))
           [0 pots]))

;;(sum (nth (pot-generations pots rules) 20))

;; (sum (second (nth (pot-generations pots rules) 20)))

;;
;; Star 2
;;
;; The sum "converges" to increasing by 40 per generation
(let [at-100 (sum (second (nth (pot-generations pots rules) 100)))]
  (+ at-100
     (* 40
        (- 50000000000
           100))))
