(ns aoc2018.parser)

(defn run [parser input]
  (parser input))

(def result first)
(def input second)

(defn parse-nothing [success-value]
  (fn [input]
    [success-value input]))

(defn parse-char-if [pred]
  (fn [[c & cs]]
    (if (and c (pred c))
      [c cs]
      nil)))

(defn parse-char
  "Parse the given character"
  [c]
  (parse-char-if (fn [input-c] (= input-c c))))

(def parse-digit
  (parse-char-if (set "0123456789")))

(def parse-hex-digit
  (parse-char-if (set "0123456789abcdefABCDEF")))

(def parse-whitespace-char
  (parse-char-if (set " \n\r\t")))

(defn parse-any [& parsers]
  {:pre [(pos? (count parsers))]}
  (fn [input]
    (loop [remaining-parsers parsers]
      (when-let [parser (first remaining-parsers)]
        (or (run parser input)
            (recur (rest remaining-parsers)))))))

(defn parse-seq [& parsers]
  {:pre [(pos? (count parsers))]}
  (fn [input]
    (loop [remaining-parsers parsers
           results           []
           remaining-input   input]
      (if-let [parser (first remaining-parsers)]
        (when-let [[result new-remaining-input] (run parser remaining-input)]
          (recur (rest remaining-parsers)
                 (conj results result)
                 new-remaining-input))
        [results remaining-input]))))

(defn interpret-as [f parser]
  (fn [input]
    (when-let [[result remaining-input] (run parser input)]
      [(f result) remaining-input])))

(defn char-seq->string [char-seq]
  (apply str char-seq))

(defn parse-string [s]
  (interpret-as char-seq->string
                (apply parse-seq (map parse-char s))))

(defn parse?
  "Runs the given parser once, returns default value on failure"
  [parser & [default]]
  (parse-any parser
             (parse-nothing default)))

(defn parse*
  "Runs the given parser until it fails, returns a vector of parse
  results"
  [parser]
  (fn [input]
    (loop [result []
           in     input]
      (if (nil? in)
        [result nil]
        (if-let [[new-result rest-input] (run parser in)]
          (recur (conj result new-result) rest-input)
          [result in])))))

(defn parse+
  "Same as parse*, but must succeed at least once"
  [parser]
  (interpret-as (fn [[result results]]
                  (concat [result] results))
                (parse-seq parser
                           (parse* parser))))

(defn parse-n
  "Runs the given parser exactly n times."
  [n parser]
  {:pre [(pos? n)]}
  (fn [input]
    (loop [results []
           in      input
           n-left  n]
      (if (zero? n-left)
        [results in]
        (when-let [[new-result rest-input] (run parser in)]
          (recur (conj results new-result)
                 rest-input
                 (dec n-left)))))))

(defn parse-as [value parser]
  (interpret-as (constantly value)
                parser))

(defmacro let-parses*
  [input bindings & body]
  (if (zero? (count bindings))
    [`(do ~@body) input]
    (let [[bind-form p] (take 2 bindings)]
      `(when-let [[~bind-form rest-input#] (run ~p ~input)]
         (let-parses* rest-input#
                      ~(drop 2 bindings)
                      ~@body)))))

(defmacro let-parses
  [bindings-vec & body]
  `(fn [input#]
     (let-parses* input# ~bindings-vec ~@body)))

(defn skip-whitespace [parser]
  (let-parses [_      (parse* parse-whitespace-char)
               result parser
               _      (parse* parse-whitespace-char)]
    result))

(defn parse-joined-by [p-joined p]
  (let-parses [result  p
               results (->> (parse-seq p-joined p)
                            (interpret-as second)
                            (parse*))]
    (into [] (concat [result] results))))

(defn parse-lines [p]
  (parse-joined-by (parse+ parse-whitespace-char) p))

(defn parse-delimited-by
  ([p-sides p] (parse-delimited-by p-sides p-sides p))
  ([p-start p-end p]
   (let-parses [_      p-start
                result (parse? p [])
                _      p-end]
     result)))

(defn char-range [char-start char-end-inclusive]
  (map char
       (range (int char-start)
              (inc (int char-end-inclusive)))))

(defn parse-char-range [char-start char-end-inclusive]
  (parse-char-if (set (char-range char-start char-end-inclusive))))

(def parse-lowercase-char (parse-char-range \a \z))
(def parse-uppercase-char (parse-char-range \A \Z))

;;;
;;; JSON parser
;;;

;; Null

(def json-null
  (parse-as nil (parse-string "null")))

;; Boolean

(def json-boolean
  (parse-any (parse-as true  (parse-string "true"))
             (parse-as false (parse-string "false"))))

;; Helpers

(def digit->int
  (merge (into {} (map vector
                       "0123456789"
                       (range 0 10)))
         (into {} (map vector
                       "abcdef"
                       (range 10 16)))
         (into {} (map vector
                       "ABCDEF"
                       (range 10 16)))))

(defn digit-string->int [base digit->int digit-string]
  (reduce (fn [acc digit]
            (+ (* acc base)
               (digit->int digit)))
          (bigint 0)
          digit-string))

(def hex-string->int (partial digit-string->int 16 digit->int))

(def parse-unicode
  (let-parses [_      (parse-string "\\u")
               digits (parse-n 4 parse-hex-digit)]
    (->> digits
         (apply str)
         (hex-string->int)
         (char))))

(def json-escape-strings
  (map (partial str "\\")
       "\"\\/bfnrtu"))

(def json-escape-string-to-char
  {"\\\\" \\
   "\\\"" \"
   "\\/"  \/
   "\\b"  \backspace
   "\\f"  \formfeed
   "\\n"  \newline
   "\\r"  \return
   "\\t"  \tab})

(def parse-json-escape-string
  (parse-any parse-unicode
             (->> json-escape-strings
                  (map parse-string)
                  (map (partial interpret-as json-escape-string-to-char))
                  (apply parse-any))))

;; String

(def json-string
  (interpret-as (partial apply str)
                (parse-delimited-by
                 (parse-char \")
                 (parse* (parse-any parse-json-escape-string
                                    (parse-char-if (complement #{\\ \"})))))))

(def parse-zero (interpret-as digit->int (parse-char \0)))

(def decimal-string->int (partial digit-string->int 10 digit->int))

(def parse-nat (parse-any parse-zero
                          (interpret-as decimal-string->int
                                        (parse+ parse-digit))))

(def parse-int
  (let-parses [minus (parse? (parse-char \-))
               _     (parse? (parse-char \+)) ; Not actually allowed?
               nat   parse-nat]
    (if minus (- nat) nat)))

(defn decimal-string->frac [string]
  (->> string
       (reverse)
       (apply str)
       (digit-string->int 1/10 digit->int)
       (* 1/10)))

(def parse-frac
  (let-parses [_ (parse-char \.)
               frac (interpret-as decimal-string->frac
                                  (parse+ parse-digit))]
    frac))

(def parse-exponent
  (let-parses [_ (parse-char-if (set "eE"))
               sign (parse? (parse-char-if (set "+-")))
               exp (interpret-as decimal-string->int
                                 (parse+ parse-digit))]
    (apply * (repeat exp (if (= sign \-) 1/10 (bigint 10))))))

(def json-number
  (let-parses [integer  parse-int
               fraction (parse? parse-frac 0)
               exp      (parse? parse-exponent 1)]
    (* (+ integer fraction)
       exp)))


;; Object

(declare json-value)

(def kv-pair
  (let-parses [k (->> json-string
                      (interpret-as keyword)
                      (skip-whitespace))
               _ (parse-char \:)
               v json-value]
    [k v]))

(def json-object
  (->> kv-pair
       (parse-joined-by    (parse-char \,))
       (parse-delimited-by (parse-char \{)
                           (parse-char \}))
       (interpret-as (partial into {}))))

(def json-array
  (->> json-value
       (parse-joined-by    (parse-char \,))
       (parse-delimited-by (parse-char \[)
                           (parse-char \]))
       (interpret-as (partial into []))))

(def json-value
  (skip-whitespace (parse-any json-string
                              json-number
                              json-object
                              json-array
                              json-boolean
                              json-null)))
