(ns aoc2018.day7
  (:require [aoc2018.parser :refer :all]
            [aoc2018.util :refer :all]))

(def parse-dependency
  (let-parses [_ (parse-string "Step ")
               pre parse-any-char
               _ (parse-string " must be finished before step ")
               post parse-any-char
               _ (parse-string " can begin.")]
    [pre post]))

(def parse-dependencies
  (parse-lines parse-dependency))

(def dependencies (result (run parse-dependencies (slurp "resources/day7.txt"))))

(defn empty-dependency-map [dependencies]
  (zipmap (distinct (mapcat identity dependencies))
          (repeat {:pre #{} :post #{}})))

(defn dependency-map [dependencies]
  (reduce (fn [deps [pre-char post-char]]
            (-> deps
                (update-in [post-char :pre] conj pre-char)
                (update-in [pre-char :post] conj post-char)))
          (empty-dependency-map dependencies)
          dependencies))

(defn first-steps [dep-map]
  (->> dep-map
       (filter (comp empty? :pre second))
       (map first)))

(defn preconditions-met? [taken-steps dep-map step]
  (every? (set taken-steps)
          (:pre (get dep-map step))))


(defn steps [dep-map]
  (loop [q (first-steps dep-map)
         taken-steps []]
    (let [[step new-q] (peek-pop q)]
      (cond (nil? step)
            (apply str taken-steps)

            (> (count taken-steps)
               (count dep-map))
            :fail

            (not (preconditions-met? taken-steps dep-map step))
            (recur new-q
                   taken-steps)

            :else
            (recur (->> (concat new-q (:post (get dep-map step)))
                        (remove (set taken-steps))
                        distinct
                        sort)
                   (conj taken-steps step))))))

;; (steps (dependency-map dependencies))

;;
;; Star 2
;;
(defn duration [step]
  (+ 60 (- (int step) 64)))

(defn workers-available? [{:keys [work-items]}]
  (< (count work-items) 5))

(defn work-available? [{:keys [work-available]}]
  (pos? (count work-available)))

(defn assign-work [work-context]
  (if (and (work-available? work-context)
           (workers-available? work-context))
    (recur (let [[step work-available] (peek-pop (:work-available work-context))]
             (-> work-context
                 (update :work-items conj {:step step
                                           :time-left (duration step)
                                           :post (:post (get (:dep-map work-context)
                                                             step))})
                 (assoc :work-available work-available))))
    work-context))

(defn work-finished? [{:keys [work-items]}]
  (some (complement pos?) (map :time-left work-items)))

(defn finish-work [work-context]
  (if (work-finished? work-context)
    (let [{finished false
           non-finished true} (group-by (comp pos? :time-left)
                                        (:work-items work-context))
          work-done (concat (:work-done work-context)
                            (->> finished
                                 (sort-by :time-left)
                                 (map :step)))]
      (-> work-context
          (assoc :work-items non-finished)
          (assoc :work-done work-done)
          (update :work-available (fn [work-available]
                                    (->> (concat work-available
                                                 (mapcat :post finished))
                                         distinct
                                         (filter (partial preconditions-met?
                                                          work-done
                                                          (:dep-map work-context)))
                                         sort)))))
    work-context))

(defn advance-time [work-context]
  ;; TODO Dummy implementation
  (let [time (if (pos? (count (:work-items work-context)))
               (find-min (mapv :time-left (:work-items work-context)))
               1)]
    (-> work-context
       (update :time + time)
       (update :work-items (fn [items]
                             (mapv #(update % :time-left - time)
                                   items))))))

(defn all-done? [work-context]
  (and (empty? (:work-available work-context))
       (empty? (:work-items work-context))))

(defn parallel-steps [dep-map]
  (loop [work-context {:time -1
                       :dep-map dep-map
                       :work-available (first-steps dep-map)
                       :work-items []
                       :work-done []}]
    (println (:time work-context)
             (mapv :step (:work-items work-context))
             (vec (:work-done work-context)))
    (cond (all-done? work-context)
          (apply str (:work-done work-context))

          (> (count (:work-done work-context))
             (count dep-map))
          :fail

          :else
          (recur (-> work-context
                     finish-work
                     assign-work
                     advance-time)))))

;; (parallel-steps (dependency-map dependencies))
