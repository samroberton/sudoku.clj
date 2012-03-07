(ns sudoku.core
  (:use [clojure.string :only [join]]))

(def +digits+ #{\1 \2 \3 \4 \5 \6 \7 \8 \9})

;;;
;;; Representation
;;;

(defn- idx->coord
  "Converts an index (0-80) to a pair of one-based [row col] coordinates into the puzzle grid."
  [idx]
  [(inc (/ idx 9)) (inc (rem idx 9))])

(defn- coord->idx
  "Converts a pair of one-based [row col] coordinates to a 0-80 index into the puzzle
representation."
  [row col]
  (+ (* (dec row) 9) (dec col)))

(defn- digit-at
  "Get the digit at the given index or coordinates in the puzzle representation."
  ([puzzle idx]
     (puzzle idx))
  ([puzzle row col]
     (puzzle (coord->idx row col))))

(defn- add-digit
  "Adds the given digit to the puzzle representation at the given index, returning a new puzzle
representation."
  [puzzle idx d]
  (let [val (cond (= \. d) nil
                  (+digits+ d) d
                  :else (throw (Exception. (str "Character at coordinates [" (idx->coord idx)
                                                "] is '" d "'; expected a digit or '.'."))))]
    (if val (assoc puzzle idx val) puzzle)))

(defn extract-row
  [puzzle row]
  (map #(digit-at puzzle row %1) (range 1 10)))

(defn extract-col
  [puzzle col]
  (map #(digit-at puzzle %1 col) (range 1 10)))

(defn extract-box
  [puzzle box]
  (let [origin ({1 0,
                 2 3,
                 3 6,
                 4 27,
                 5 30,
                 6 33,
                 7 54,
                 8 57,
                 9 60} box)]
    (map #(digit-at puzzle (+ %1 origin))
         [0 1 2 9 10 11 18 19 20])))

(defn str->puzzle
  "Take a string of 9 newline separated rows of 9 digits, with '.' for an as-yet-undetermined digit,
and convert it to the program's internal representation of a Sudoku puzzle (a map of 0-80 to
Integers)."
  [str]
  (let [s (.replaceAll str "\n" "")]
    (loop [idx    0
           puzzle {}]
      (if (= idx (.length s))
        puzzle
        (recur (inc idx) (add-digit puzzle idx (.charAt s idx)))))))

(defn puzzle->str
  "Get the puzzle representation as a string."
  [puzzle]
  (join \newline
        (map (fn [row]
               (join (map (fn [col]
                            (or (digit-at puzzle row col) \.))
                          (range 1 10))))
             (range 1 10))))


;;;
;;; Solution checking
;;;

(defn complete?
  "True if all positions in the puzzle have a digit."
  [puzzle]
  (every? #(puzzle %1) (range 0 81)))

(defn- find-dups
  "Identify digits which are present more than once in the row/col/box (specified by using
extract-row, extract-col or extract-box as the extract-fn) 'n'. Returns nil if there are no
duplicates in the row/col/box."
  [puzzle extract-fn n]
  (not-empty
   (filter (fn [d]
             (< 1 (count (filter #(= d %1) (extract-fn puzzle n)))))
           +digits+)))

(defn errors
  "Returns a seq of error messages, which is empty if there are no errors in the puzzle."
  [puzzle]
  (filter identity
          (for [[f name] [[extract-row "row"] [extract-col "column"] [extract-box "box"]]
                n        (range 1 10)]
            (let [dups (find-dups puzzle f n)]
              (when dups
                (str "Duplicate digit(s) in " name " " n ": " (join ", " dups)))))))

(defn solved?
  "True if the puzzle is solved."
  [puzzle]
  (and (empty? (errors puzzle))
       (complete? puzzle)))


;;;
;;; Solution generation
;;;

(defn- find-avail
  "Identify digits which are not present in the given sequence."
  [digits]
  (filter #(not (digits %1)) +digits+))

