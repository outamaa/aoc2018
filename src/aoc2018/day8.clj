(ns aoc2018.day8
  (:require [aoc2018.parser :refer :all]
            [aoc2018.util :refer :all]))

(def parse-num
  (let-parses [n json-number
               _ parse-whitespace-char]
    n))

(def parse-node
  (let-parses [n-children parse-num
               n-metadata parse-num
               children (parse-n n-children
                                 parse-node)
               metadata (interpret-as (partial mapv int)
                                      (parse-n n-metadata
                                               parse-num))]
    {:metadata metadata
     :children children}))

(def tree (result (run parse-node (slurp "resources/day8.txt"))))

(defn metadata-sum [node]
  (reduce + (:metadata node)))

(defn sum-node [node]
  (reduce + (conj (map sum-node (:children node))
                  (metadata-sum node))))

;;
;; Star 2
;;
(defn childless? [node]
  (empty? (:children node)))

(defn children-by-metadata [node]
  (->> (:metadata node)
       (map (fn [idx]
              (get (:children node)
                   (dec idx))))
       (remove nil?)))

(declare node-value)

(defn child-sum [node]
  (reduce +
          (map node-value
               (children-by-metadata node))))

(defn node-value [node]
  (if (childless? node)
    (metadata-sum node)
    (child-sum node)))
