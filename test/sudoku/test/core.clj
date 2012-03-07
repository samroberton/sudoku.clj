(ns sudoku.test.core
  (:use [sudoku.core])
  (:use [clojure.test]))

(def +rows+ "111111111
222222222
333333333
444444444
555555555
666666666
777777777
888888888
999999999")

(def +cols+ "123456789
123456789
123456789
123456789
123456789
123456789
123456789
123456789
123456789")

(def +boxes+ "111222333
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
       +rows+
       +cols+
       +boxes+))

(deftest test-extract-row
  (are [x] (= (extract-row (str->puzzle +rows+) x) (repeat 9 (.charAt (str x) 0)))
       1
       2
       3
       4
       5
       6
       7
       8
       9))

(deftest test-extract-col
  (are [x] (= (extract-col (str->puzzle +cols+) x) (repeat 9 (.charAt (str x) 0)))
       1
       2
       3
       4
       5
       6
       7
       8
       9))

(deftest test-extract-box
  (are [x] (= (extract-box (str->puzzle +boxes+) x) (repeat 9 (.charAt (str x) 0)))
       1
       2
       3
       4
       5
       6
       7
       8
       9))

(def +incomplete+ "123456789
123456789
123456.89
123456789
123456789
123456789
123456789
123456789
123456789")

(deftest test-complete?
  (are [p complete] (= (complete? (str->puzzle p)) complete)
       +rows+ true
       +cols+ true
       +boxes+ true
       +incomplete+ false))

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
  (is (= (errors (str->puzzle +e1+)) ["Duplicate digit(s) in row 1: 1"]))
  (is (= (errors (str->puzzle +e2+)) ["Duplicate digit(s) in row 3: 2, 4"
                                      "Duplicate digit(s) in column 9: 8"
                                      "Duplicate digit(s) in box 2: 4"
                                      "Duplicate digit(s) in box 5: 5"])))
