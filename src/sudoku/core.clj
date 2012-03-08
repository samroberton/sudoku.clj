(ns sudoku.core
  (:use [clojure.string :only [join]])
  (:use [clojure.set :only [intersection]]))

;;
;; ## Utilities
;;

(defn map-when
  "Like map, but discard results which are nil from the mapped collection."
  [f coll]
  (filter identity (map f coll)))


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
  "Return a flat seq of the [row column] coordinates of all cells which don't have a value."
  [puzzle]
  (filter #(not (value puzzle %1)) +cells+))

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
                {:cell cell :value (first (candidates puzzle cell))}))
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
                  {:cell [row col] :exclude excluded})))
            (unknown-cells puzzle)))

(defn only-available-cell-for-value-rule
  "Checks each row, column and box in turn, for each value, looking for any cell which is the only
possible remaining place for that value."
  [puzzle]
  (letfn [(find-possible-cells-for-value [cells val]
            (map-when #(if (contains? (candidates puzzle %1) val) %1) cells))
          (find-forced-cells [cells]
            (not-empty (map-when #(let [poss (find-possible-cells-for-value cells %1)]
                                    (when (= 1 (count poss))
                                      {:cell (first poss) :value %1}))
                                 (filter (fn [val]
                                           (not-any? #(= val (value puzzle %1)) cells))
                                         +values+))))
          (find-forced-cells-in-colls [collection]
            (apply concat (map-when find-forced-cells collection)))]
    (seq (into #{} (concat (find-forced-cells-in-colls +rows+)
                           (find-forced-cells-in-colls +cols+)
                           (find-forced-cells-in-colls +boxes+))))))

(def +inference-rules+ [only-one-candidate-rule
                        eliminate-known-values-rule
                        only-available-cell-for-value-rule])

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
  (first (map-when #(not-empty (%1 puzzle)) +inference-rules+)))

(defn solve
  "Solve the puzzle, if possible. Return `[solved? solution]`, where `solved?` is true/false, and
`solution` is the completed solution (if solved) or as far as we could get (if not)."
  [puzzle]
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
              (doseq [{[row col] :cell val :value exclusions :exclude} inferences]
                (println (str "\trow " row ", col " col ": "
                              (if val
                                (str "set to " val)
                                (str "exclude " (join ", " exclusions)))))))
            (solve
             (reduce (fn [puzzle {cell :cell val :value exclusions :exclude}]
                       (if val
                         (assoc-value puzzle cell val)
                         (exclude-candidates puzzle cell exclusions)))
                     puzzle
                     inferences))))))
