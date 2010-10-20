(ns sudoku-tests
  (:use [sudoku])
  (:use [clojure.test :only (deftest is run-tests)]))

(deftest sudoku-tests
  (is (= (count squares) 81))
  (is (= (count unitlist) 27))
  (is (every? true? (for [s squares] (= (count (units s)) 3))))
  (is (every? true? (for [s squares] (= (count (peers s)) 20))))
  (is (= (units "C2") '(("A2" "B2" "C2" "D2" "E2" "F2" "G2" "H2" "I2")
			("C1" "C2" "C3" "C4" "C5" "C6" "C7" "C8" "C9")
			("A1" "A2" "A3" "B1" "B2" "B3" "C1" "C2" "C3"))))
  (is (= (peers "C2") #{"A2" "B2" "D2" "E2" "F2" "G2" "H2" "I2"
			"C1" "C3" "C4" "C5" "C6" "C7" "C8" "C9"
			"A1" "A3" "B1" "B3"})))

(run-tests)
