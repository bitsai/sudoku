(ns sudoku
  (:use algorithm))

(let [file (first *command-line-args*)]
  (display (solve-file file)))
