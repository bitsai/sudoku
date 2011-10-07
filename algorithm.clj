(ns algorithm
  (:require [clojure.string :as str]))

(defn cross [as bs]
  (for [a as, b bs] (str a b)))

(defn all? [coll]
  (every? identity coll))

(defn copy [values]
  (atom @values))

(def digits (set "123456789"))
(def rows "ABCDEFGHI")
(def cols digits)
(def squares (cross rows cols))
(def unitlist
  (concat
   (for [c cols] (cross rows [c]))
   (for [r rows] (cross [r] cols))
   (for [rs (partition 3 rows), cs (partition 3 cols)] (cross rs cs))))
(def units
  (into {} (for [s squares] [s (filter #(some #{s} %) unitlist)])))
(def peers
  (into {} (for [s squares] [s (disj (set (flatten (units s))) s)])))

(declare assign eliminate helper-1 helper-2)

(defn assign [values s d]
  (let [other-values (disj (@values s) d)]
    (if (every? #(eliminate values s %) other-values)
      values)))

(defn eliminate [values s d]
  (if-not ((@values s) d)
    values
    (let [other-values (disj (@values s) d)]
      (swap! values assoc s other-values)
      (cond
       (not (helper-1 values s)) false
       (not (helper-2 values s d)) false
       :else values))))

;; If a square s is reduced to 1 value d2, then eliminate d2 from peers.
(defn helper-1 [values s]
  (case (count (@values s))
	0 false
	1 (let [d2 (first (@values s))]
            (every? #(eliminate values % d2) (peers s)))
	true))

;; If a unit u is reduced to 1 place for a value d, then put it there.
(defn helper-2 [values s d]
  (all? (for [u (units s)]
	  (let [dplaces (filter #((@values %) d) u)]
	    (case (count dplaces)
		  0 false
		  1 (assign values (first dplaces) d)
		  true)))))

(defn grid-values [grid]
  (zipmap squares grid))

(defn parse-grid [grid]
  (let [values (atom (zipmap squares (repeat digits)))]
    (if (all? (for [[s d] (grid-values grid) :when (digits d)]
                (assign values s d)))
      values)))

(defn search [values]
  (cond
   (not values) false
   (every? #(= 1 (count (@values %))) squares) values
   :else (let [unfilled (filter #(> (count (@values %)) 1) squares)
	       s (apply min-key #(count (@values %)) unfilled)]
	   (some #(search (assign (copy values) s %)) (@values s)))))

(defn solve [grid]
  (search (parse-grid grid)))

(defn solve-file [file]
  (let [grid (remove #{\newline \return} (slurp file))]
    (solve grid)))
