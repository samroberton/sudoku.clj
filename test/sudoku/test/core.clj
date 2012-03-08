(ns sudoku.test.core
  (:use [sudoku.core])
  (:use [clojure.test]))

(def +by-rows+ "111111111
222222222
333333333
444444444
555555555
666666666
777777777
888888888
999999999")

(def +by-cols+ "123456789
123456789
123456789
123456789
123456789
123456789
123456789
123456789
123456789")

(def +by-boxes+ "111222333
111222333
111222333
444555666
444555666
444555666
777888999
777888999
777888999")

(deftest str->puzzle->str
  (are [p] (= p (puzzle->str (str->puzzle p)))
       +by-rows+
       +by-cols+
       +by-boxes+))

(def +incomplete+ "123456789
123456789
123456.89
123456789
123456789
123456789
123456789
1234.6789
123456789")

(deftest test-complete?
  (are [p complete] (= (complete? (str->puzzle p)) complete)
       +by-rows+ true
       +by-cols+ true
       +by-boxes+ true
       +incomplete+ false))

(deftest test-unknowns
  (is (= [[2 6] [7 4]] (unknown-cells (str->puzzle +incomplete+)))))

(def +e1+ "1.......1
.........
.........
.........
.........
.........
.........
.........
.........")

(def +e2+ "........8
.........
2..4.4..2
....5....
...5.....
.........
........8
.........")

(deftest test-errors
  (is (= (errors (str->puzzle +e1+)) ["Duplicate digit(s) in row 0: 1"]))
  (is (= (errors (str->puzzle +e2+)) ["Duplicate digit(s) in row 2: 2, 4"
                                      "Duplicate digit(s) in column 8: 8"
                                      "Duplicate digit(s) in box 1: 4"
                                      "Duplicate digit(s) in box 4: 5"])))

(def +simple1-prob+ ".4..2..7.
...4.59..
8.....364
16..8429.
2...1...6
.9725..38
536.....7
..21.7...
.1..6..2.")

(def +simple1-soln+ "349628571
671435982
825971364
163784295
258319746
497256138
536892417
982147653
714563829")

(deftest simple1-solved?
  (is (= (solved? (str->puzzle +simple1-prob+)) false))
  (is (= (solved? (str->puzzle +simple1-soln+)) true)))

(deftest solve-simple1
  (is (= (puzzle->str (second (solve (str->puzzle +simple1-prob+)))) +simple1-soln+)))

(def +hard1-prob+ "..9.8..1.
..5.....2
1...2.853
6.1......
8..3.6..1
......3.7
467.1...5
5.....1..
.1..3.9..")
