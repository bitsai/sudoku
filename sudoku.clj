(ns sudoku
  (:use algorithm))

(display (solve-file (first *command-line-args*)))
