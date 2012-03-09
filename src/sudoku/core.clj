(ns sudoku.core
  (:use [clojure.string :only [join]])
  (:use [clojure.set :only [intersection subset?]]))

;;
;; ## Utilities
;;

(defn map-when
  "Like map, but discard results which are nil from the mapped collection."
  [f & colls]
  (filter identity (apply map f colls)))


;;
;; ## Representation
;;
;; A puzzle contains nine rows and nine columns, for a total of 81 cells. The nine three by three
;; grids within each puzzle are referred to as 'boxes'. The digit in a cell is referred to as its
;; value, and values are represented as characters (not as integers). No distinction is made between
;; values which are supplied as part of the puzzle and values which are identified during the
;; generation of the solution.
;;
;; The rows, columns and boxes are each numbered from zero to eight. Rows are numbered top-down,
;; columns left-to-right, and boxes left-to-right and then top-to-bottom (as though read in
;; English).
;;
;; Puzzles are entered and printed as strings in the following form:
;;
;; <pre><code>
;; .4..2..7.
;; ...4.59..
;; 8.....364
;; 16..8429.
;; 2...1...6
;; .9725..38
;; 536.....7
;; ..21.7...
;; .1..6..2.
;; </code></pre>
;;
;; where dots represent cells with unknown values.
;;

(def +values+
  "The set of possible values in a cell."
  #{\1 \2 \3 \4 \5 \6 \7 \8 \9})

(def +cells+
  "A flat seq of all the cells in a puzzle, with all the cells in row one followed by all the cells
in row two, etc."
  (for [r (range 9) c (range 9)] [r c]))

(def +rows+
  "A sorted mapping from row number to seq of [row column] coordinates of the cells in that row."
  (into [] (for [r (range 9)]
             (into [] (for [c (range 9)]
                        [r c])))))

(def +cols+
  "A sorted mapping from column number to seq of [row column] coordinates of the cells in that
column."
  (into [] (for [c (range 9)]
             (into [] (for [r (range 9)]
                        [r c])))))

(def +boxes+
  "A sorted mapping from box number to seq of [row column] coordinates of the cells in that box."
  (into [] (for [origin-r (range 0 9 3)
                 origin-c (range 0 9 3)]
             (into [] (for [offset-r (range 3)
                            offset-c (range 3)]
                        [(+ origin-r offset-r) (+ origin-c offset-c)])))))

(def +cell-to-box+
  "A mapping from [row column] coordinates to the box which that cell occupies."
  (into {} (apply concat (map-indexed (fn [index cells]
                                        (map (fn [cell] [cell index])
                                             cells))
                                      +boxes+))))

(defn value
  "Get the value in the cell at the given row and column, or nil if there isn't a value specified
for that cell yet."
  [puzzle [row col]]
  (:value (get puzzle [row col])))

(defn candidates
  "Get the set of possible candidates for the cell a given row and column. If there is a defined
value for the cell already, return the empty set."
  [puzzle [row col]]
  (if (value puzzle [row col])
    #{}
    (:candidates (get puzzle [row col]))))

(defn assoc-value
  "Associates the specified value with the cell at the given row and column, returning a new puzzle
representation."
  [puzzle [row col] val]
  (assoc puzzle [row col] {:value val}))

(defn exclude-candidates
  "Excludes the specified set of values as candidates for the cell at the given  row and column,
returning a new puzzle representation."
  [puzzle [row col] vals]
  (assoc puzzle [row col] {:candidates (apply disj (candidates puzzle [row col]) vals)}))

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
               (let [c   (.charAt s idx)
                     row (int (/ idx 9))
                     col (rem idx 9)]
                 (cond (= c \.)     (assoc puzzle [row col] {:candidates +values+})
                       (+values+ c) (assoc-value puzzle [row col] c)
                       :else        (throw
                                     (Exception.
                                      (str "Character at coordinates [" row " " col
                                           "] is '" c "'; expected a digit or '.'."))))))))))

(defn puzzle->str
  "Get the puzzle representation as a string."
  [puzzle]
  (join \newline (map (fn [cells]
                        (join (map #(or (value puzzle %1) \.) cells)))
                      +rows+)))


;;
;; ## Solution checking
;;

(defn- find-dups
  "Identify values which are present more than once in the cells at the given coordinates. Returns
nil if there are no duplicates in those cells."
  [puzzle cells]
  (not-empty
   (filter (fn [val]
             (< 1 (count (filter #(= val %1) (map #(value puzzle %1) cells)))))
           +values+)))

(defn errors
  "Returns a seq of error messages, which is empty if there are no errors in the puzzle."
  [puzzle]
  (filter identity
          (for [[cells-map name] [[+rows+ "row"] [+cols+ "column"] [+boxes+ "box"]]
                n        (range 9)]
            (let [dups (find-dups puzzle (cells-map n))]
              (when dups
                (str "Duplicate digit(s) in " name " " n ": " (join ", " dups)))))))

(defn unknown-cells
  "Return a flat seq of the [row column] coordinates of all of the given cells which don't have a
value."
  ([puzzle]
     (unknown-cells puzzle +cells+))
  ([puzzle cells]
     (filter #(not (value puzzle %1)) cells)))

(defn unplaced-values
  "Where `cells` is vector of the coordinates of all cells in a row, column or box, returns a set of
the values which have not yet been assigned to a cell in that row, column or box."
  [puzzle cells]
  (filter (fn [val]
            (not-any? #(= val (value puzzle %1)) cells))
          +values+))

(defn complete?
  "True if all cells in the puzzle have a value."
  [puzzle]
  (empty? (unknown-cells puzzle)))

(defn solved?
  "True if the puzzle is solved."
  [puzzle]
  (and (empty? (errors puzzle)) (complete? puzzle)))


;;
;; ## Inference rules
;;
;; The inference rules each take a puzzle and return a seq of 'next steps'.  Each 'next step' is a
;; map of the form `{:cell cell :value <value>}` or `{:cell cell :exclude #{<values>}}`.
;;

(defn only-one-candidate-rule
  "Cells with only one candidate must have that as their value."
  [puzzle]
  (map-when (fn [cell]
              (when (= 1 (count (candidates puzzle cell)))
                {:cell   cell
                 :value  (first (candidates puzzle cell))
                 :reason (str "Cell " cell " has " (candidates puzzle cell)
                              " as its only possible value.")}))
            (unknown-cells puzzle)))

(defn eliminate-known-values-rule
  "Eliminates from the candidate set for a cell all known values from the rest of its row, column
and box."
  [puzzle]
  (map-when (fn [[row col]]
              (let [excluded (intersection (candidates puzzle [row col])
                                           (into #{} (map-when #(value puzzle %1)
                                                 (concat (+rows+ row)
                                                         (+cols+ col)
                                                         (+boxes+ (+cell-to-box+ [row col]))))))]
                (when-not (empty? excluded)
                  {:cell [row col]
                   :exclude excluded
                   :reason  (str "Values " excluded " are already present in the same row, column "
                                 "and/or box as " [row col] ".")})))
            (unknown-cells puzzle)))

(defn- combine-seqs
  "`concat` a seq of seqs into a single seq, ignoring nil/empty members, and producing `nil` if
there are no members at all."
  [seq-of-seqs]
  (not-empty (apply concat seq-of-seqs)))

(defn- each-row-column-and-box
  "Calls `(f puzzle cells description-of-cells)` once for each row, column and box (where `cells` is
the coordinates of the cells in that row, column or box, and where `description-of-cells` is eg
`\"row 1\"`).  `f` is expected to return a seq of 'next steps'.  Combines the results of all calls
to `f` into a single seq, eliminating duplicate results."
  [f puzzle]
  (letfn [(apply-to-cells [cells cells-type-str]
            (combine-seqs (map-when #(f puzzle %1 (str cells-type-str " " %2))
                                    cells
                                    (range (count cells)))))]
    (seq (into #{} (concat (apply-to-cells +rows+ "row")
                           (apply-to-cells +cols+ "col")
                           (apply-to-cells +boxes+ "box"))))))

(defn only-available-cell-for-value-rule
  "Checks each row, column and box in turn, for each value, looking for any cell which is the only
possible remaining place for that value."
  [puzzle]
  (letfn [(find-possible-cells-for-value [cells val]
            (map-when #(if (contains? (candidates puzzle %1) val) %1) cells))
          (find-in-cells [puzzle cells description-of-cells]
            (not-empty (map-when #(let [poss (find-possible-cells-for-value cells %1)]
                                    (when (= 1 (count poss))
                                      {:cell   (first poss)
                                       :value  %1
                                       :reason (str "Cell " %1 " is the only cell in "
                                                    description-of-cells " which can have " val
                                                    " as its value.")}))
                                 (unplaced-values puzzle cells))))]
    (each-row-column-and-box find-in-cells puzzle)))

(defn n-cells-with-only-n-candidates-rule
  "Finds a group of `n` cells in a row, column or box, which have only `n` candidates between them,
so that we can conclude that those candidates cannot be placed in any other cells in that row,
column or box.  For example, if two cells in a box have only 3 and 4 as candidates, then the 3 and 4
are necessarily in those two cells, and cannot be in any other cells in the box."
  [puzzle]
  (letfn [(find-exclusions-based-on-cell [description-of-cells unknowns cell]
            (let [cands                       (candidates puzzle cell)
                  other-cells-with-same-cands (filter #(subset? (candidates puzzle %1) cands)
                                                      (disj unknowns cell))]
              (when (= (count cands)
                       (+ 1 (count other-cells-with-same-cands)))
                (not-empty
                 (map-when (fn [c]
                             (when-let [vals-to-exclude (not-empty
                                                         (intersection (candidates puzzle c)
                                                                       cands))]
                               {:cell    c
                                :exclude vals-to-exclude
                                :reason  (str "Cells " (conj other-cells-with-same-cands cell)
                                              " have values " cands " between them, so no other "
                                              "cells in " description-of-cells " can have any of "
                                              "those values.  So we can exclude " vals-to-exclude
                                              " from cell " c "'s list of candidates (currently "
                                              (candidates puzzle c) ").")}))
                           (apply disj unknowns cell other-cells-with-same-cands))))))
          (find-in-cells [puzzle cells description-of-cells]
            (let [unknowns (into #{} (unknown-cells puzzle cells))]
              (not-empty
               (apply concat
                      (map-when #(find-exclusions-based-on-cell description-of-cells unknowns %1)
                                unknowns)))))]
    (each-row-column-and-box find-in-cells puzzle)))

(defn n-candidates-in-only-n-cells-rule
  "Finds a set of `n` unplaced values which only appear in the candidate lists of `n` cells in a
given row, column or box, so that we can conclude that those values must appear in those cells.
Therefore we can also conclude that other candidates can be removed from those cells' candidate
lists. For example, if two cells have candidate lists `#{3, 4, 7}` and `#{3, 6, 7}`, and there are
no other cells in the same row/column/box with either 3 or 7 in their candidate lists, then we can
remove the 4 and 6, since one of these cells must be the 3, and the other must be the 7.  (While
this doesn't solve the 3 or 7, it may leave us with only one remaining place for the 4 or for the
6.)"
  [puzzle]
  ;; TODO
  nil)

(defn box-line-intersection-rule
  "If the only available cells for a value in a particular row or column are within the same
box, then that value cannot appear in any cell in the box which is not in that row/column. For
example, in the following puzzle:
<pre><code>
.xx......
.xx......
.xx......
4........
5........
6........
7........
8........
9........
</code></pre>
the unknown cells in the first column must include a 1 (also a 2 and a 3).  Therefore none of the
remaining cells in box 0 can contain a 1 (nor 2 nor 3), and so 1 (and 2 and 3) can be removed from
the candidate lists of all cells marked `x`.

The same applies in reverse:  if the only available cells for a value in a particular box are in a
single row or column, then no other cells in that row or column can contain that value."
  [puzzle]
  ;; TODO
  nil)

(def +inference-rules+ [only-one-candidate-rule
                        eliminate-known-values-rule
                        only-available-cell-for-value-rule
                        n-cells-with-only-n-candidates-rule
                        n-candidates-in-only-n-cells-rule
                        box-line-intersection-rule])

;;
;; ## Put it all together...
;;

(def ^:dynamic *print-working*
  "Set to true to enable println logging of how the solution is being reached."
  false)

(defn apply-rules
  [puzzle]
  "Apply the inference rules one at a time until one of them produces a result, and return that.
This way we do everything we can with the easiest inference rule, and only apply more complicated
rules when we're otherwise stuck."
  (loop [[rule & rules] +inference-rules+]
    (let [result (not-empty (rule puzzle))]
      (cond result
            result
            (empty? rules)
            nil
            :else
            (recur rules)))))

(defn solve
  "Solve the puzzle, if possible. Return `[solved? solution]`, where `solved?` is true/false, and
`solution` is the completed solution (if solved) or as far as we could get (if not)."
  [puzzle]
  (when *print-working*
    (println (str "So far:\n" (puzzle->str puzzle))))
  (let [inferences (apply-rules puzzle)]
    (cond (solved? puzzle)
          (do
            (when *print-working*
              (println "Puzzle is solved!")
              (println (puzzle->str puzzle)))
            [true puzzle])
          (empty? inferences)
          (do
            (when *print-working*
              (println "Puzzle is not solved, but I don't know where to go from here...")
              (println (puzzle->str puzzle)))
            [false puzzle])
          :else
          (do
            (when *print-working*
              (println "Progress:")
              (doseq [{[row col] :cell val :value exclusions :exclude reason :reason} inferences]
                (println (str "\trow " row ", col " col ": "
                              (if val
                                (str "set to " val)
                                (str "exclude " (join ", " exclusions)))
                              " (reason: " reason ")"))))
            (solve
             (reduce (fn [puzzle {cell :cell val :value exclusions :exclude}]
                       (if val
                         (assoc-value puzzle cell val)
                         (exclude-candidates puzzle cell exclusions)))
                     puzzle
                     inferences))))))
