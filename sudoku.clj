(ns sudoku
  (:use algorithm))

(defn interpose-nth [n sep coll]
  (apply concat (interpose [sep] (partition n coll))))

(defn display [values]
  (let [rows (partition 9 (map #(apply str (@values %)) squares))
	lines (map #(str/join " " (interpose-nth 3 "|" %)) rows)
	separator-line (apply str (repeat 21 "-"))]
    (doseq [line (interpose-nth 3 separator-line lines)]
      (println line))))

(display (solve-file (first *command-line-args*)))
