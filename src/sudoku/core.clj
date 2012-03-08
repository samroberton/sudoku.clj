(ns sudoku.core
  (:use [clojure.string :only [join]])
  (:use [clojure.set :only [difference]]))

(def +digits+ #{\1 \2 \3 \4 \5 \6 \7 \8 \9})

;;;
;;; Representation
;;;

(defn digit-at
  "Get the digit at the given index or coordinates in the puzzle representation."
  [puzzle row col]
  (puzzle [row col]))

(defn add-digit
  "Adds the given digit to the puzzle representation at the given index, returning a new puzzle
representation."
  [puzzle row col d]
  (let [val (cond (= \. d) nil
                  (+digits+ d) d
                  :else (throw (Exception. (str "Character at coordinates [" row " " col
                                                "] is '" d "'; expected a digit or '.'."))))]
    (if val (assoc puzzle [row col] val) puzzle)))

(defn unknowns
  "Return a lazy seq of the (one-based) [x y] coordinates of all positions in the puzzle which do
not have a digit."
  [puzzle]
  (filter (fn [[row col]] (not (digit-at puzzle row col)))
          (for [row (range 1 10)
                col (range 1 10)]
            [row col])))

(defn extract-row
  [puzzle row]
  (map #(digit-at puzzle row %1) (range 1 10)))

(defn extract-col
  [puzzle col]
  (map #(digit-at puzzle %1 col) (range 1 10)))

(def +box-origins+
  [[1 1] [1 4] [1 7] [4 1] [4 4] [4 7] [7 1] [7 4] [7 7]])

(defn identify-box
  [row col]
  (loop [[origin & rem-origins] +box-origins+
         box 1]
    (if (= origin [(inc (* 3 (int (/ (dec row) 3))))
                   (inc (* 3 (int (/ (dec col) 3))))])
      box
      (recur rem-origins (inc box)))))

(defn extract-box
  [puzzle box]
  (let [[origin-row origin-col] (+box-origins+ (dec box))]
    (map (fn [[row col]] (digit-at puzzle (+ origin-row row) (+ origin-col col)))
         [[0 0] [0 1] [0 2] [1 0] [1 1] [1 2] [2 0] [2 1] [2 2]])))

(defn str->puzzle
  "Take a string of 9 newline separated rows of 9 digits, with '.' for an as-yet-undetermined digit,
and convert it to the program's internal representation of a Sudoku puzzle."
  [str]
  (let [s (.replaceAll str "\n" "")]
    (loop [idx    0
           puzzle {}]
      (if (= idx (.length s))
        puzzle
        (recur (inc idx)
               (add-digit puzzle (inc (int (/ idx 9))) (inc (rem idx 9)) (.charAt s idx)))))))

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
  (every? #(puzzle %1) (for [row (range 1 10)
                             col (range 1 10)]
                         [row col])))

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

(defn- list-avail-digits
  "Identify the digits which a given coordinate could have once other known digits from its row,
column and box are ruled out."
  [puzzle row col]
  (letfn [(digits-in [coll]
            (into #{} (filter identity coll)))]
    (difference +digits+
                (digits-in (extract-row puzzle row))
                (digits-in (extract-col puzzle col))
                (digits-in (extract-box puzzle (identify-box row col))))))

(defn next-steps
  [puzzle]
  (filter identity
          (map (fn [[row col]]
                 (let [avail (list-avail-digits puzzle row col)]
                   (when (= 1 (count avail))
                     [[row col] (first avail)])))
               (unknowns puzzle))))

(def ^:dynamic *print-working* false)

(defn solve
  [puzzle]
  (let [steps (next-steps puzzle)]
    (cond (solved? puzzle)
          (do
            (when *print-working*
              (println "Puzzle is solved!")
              (println (puzzle->str puzzle)))
            [true puzzle])
          (empty? steps)
          (do
            (when *print-working*
              (println "Puzzle is not solved, but I don't know where to go from here...")
              (println (puzzle->str puzzle)))
            [false puzzle])
          :else
          (do
            (when *print-working*
              (println "Filling in the following digits:")
              (doseq [[[row col] digit] steps]
                (println (str "\trow " row ", col " col ": " digit))))
            (loop [steps steps
                   puzzle puzzle]
              (let [[[[row col] digit] & more-steps] steps]
                (if (empty? steps)
                  (solve puzzle)
                  (recur more-steps (add-digit puzzle row col digit)))))))))
