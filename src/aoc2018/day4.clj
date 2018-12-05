(ns aoc2018.day4
  (:require [aoc2018.parser :refer :all]
            [aoc2018.util :refer :all]))

(def parse-date
  (interpret-as (partial apply str)
                (parse-n 10 parse-any-char)))

(def parse-time
  (parse-joined-by (parse-char \:)
                   (interpret-as #(Long. (apply str %))
                                 (parse-n 2 parse-digit))))

(def parse-begins-shift
  (let-parses [_ (parse-string "Guard #")
               id json-number
               _ (parse-string " begins shift")]
    {:event :begins-shift
     :guard id}))

(def parse-wakes-up
  (let-parses [_ (parse-string "wakes up")]
    {:event :wakes-up}))

(def parse-falls-asleep
  (let-parses [_ (parse-string "falls asleep")]
    {:event :falls-asleep}))

(def parse-event
  (parse-any parse-begins-shift
             parse-wakes-up
             parse-falls-asleep))

(def parse-log-line
  (let-parses [_ (parse-char \[)
               date parse-date
               _ parse-whitespace-char
               time parse-time
               _ (parse-char \])
               _ parse-whitespace-char
               event parse-event
               ]
    (merge event
           {:date date :time time})))

(def parse-logs
  (parse-lines parse-log-line))

(defn guard-ids [logs]
  (->> logs
       (reduce (fn [[guards current-guard] {next-guard :guard}]
                 (if next-guard
                   [(conj guards next-guard) next-guard]
                   [(conj guards current-guard) current-guard]))
               [[] nil])
       first))

(defn logs-with-guard-ids [logs]
  (map (fn [log-event guard-id]
         (assoc log-event :guard guard-id))
       logs
       (guard-ids logs)))

(def logs
  (->> (result (run parse-logs (slurp "resources/day4.txt")))
       (sort-by (juxt :date :time))
       logs-with-guard-ids))
;;
;; Star 1
;;

(defn events->nap [[falls-asleep-event wakes-up-event]]
  {:pre [(= (:guard falls-asleep-event)
            (:guard wakes-up-event))]}
  {:guard (:guard falls-asleep-event)
   :nap-time (- (second (:time wakes-up-event))
                (second (:time falls-asleep-event)))
   :nap-start (:time falls-asleep-event)
   :nap-end (:time wakes-up-event)})

(defn naps [logs]
  (->> logs
       (filter (comp #{:falls-asleep :wakes-up} :event))
       (partition 2)
       (map events->nap)))

(defn most-sleepy-elf [logs]
  (->> (naps logs)
       (group-by :guard)
       (map (fn [[guard naps]] [guard (reduce + 0 (map :nap-time naps))]))
       (apply max-key second)
       first))

(defn nap-minutes [nap]
  (range (second (:nap-start nap))
         (second (:nap-end nap))))


;; This is really inefficient, should iterate over minutes instead
(defn most-sleepy-minute-of-most-sleepy-elf [logs]
  (->> (naps logs)
       (filter #(= (:guard %) (most-sleepy-elf logs)))
       (mapcat nap-minutes)
       max-frequency
       first))

;; (* (most-sleepy-elf logs)
;;    (most-sleepy-minute-of-most-sleepy-elf logs))

;;
;; Star 2
;;
(defn most-frequent-sleeper [logs]
  (->> (naps logs)
       (group-by :guard)
       (map (fn [[guard naps]]
              {:guard guard
               :freq (max-frequency (mapcat nap-minutes naps))}))
       (apply max-key (comp second :freq))))

;; (let [sleeper (most-frequent-sleeper logs)]
;;   (* (:guard sleeper)
;;      (first (:freq sleeper))))
