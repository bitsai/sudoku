(ns algorithm
  (:require [clojure.string :as str])
  (:use [clojure.java.io :only (reader)])
  (:use utils))

(defn cross [A B]
  (for [a A b B] (str a b)))

(def digits "123456789")
(def rows "ABCDEFGHI")
(def cols digits)
(def squares (cross rows cols))
(def unitlist
  (concat
   (for [c cols] (cross rows (str c)))
   (for [r rows] (cross (str r) cols))
   (for [rs (partition 3 rows) cs (partition 3 cols)] (cross rs cs))))
(def units
  (into {} (for [s squares] [s (for [u unitlist :when (in? s u)] u)])))
(def peers
  (into {} (for [s squares] [s (set (remove #{s} (flatten (units s))))])))

(defn set-values! [values s s-values]
  (dosync (alter values #(assoc-in % [s] s-values))))

(declare assign eliminate helper-1 helper-2)

(defn assign [values s d]
  (let [other-values (remove #{d} (@values s))]
    (if (all? (for [d2 other-values] (eliminate values s d2)))
      values
      false)))

(defn eliminate [values s d]
  (if-not (in? d (@values s))
    values
    (let [other-values (remove #{d} (@values s))]
      (set-values! values s other-values)
      (cond
       (false? (helper-1 values s)) false
       (false? (helper-2 values s d)) false
       :else values))))

;; If a square s is reduced to 1 value d2, then eliminate d2 from the peers.
(defn helper-1 [values s]
  (case (count (@values s))
	0 false
	1 (let [d2 (first (@values s))]
	    (if-not (all? (for [s2 (peers s)] (eliminate values s2 d2)))
	      false
	      true))
	true))

;; If a unit u is reduced to only 1 place for a value d, then put it there.
(defn helper-2 [values s d]
  (all? (for [u (units s)]
	  (let [dplaces (for [s u :when (in? d (@values s))] s)]
	    (case (count dplaces)
		  0 false
		  1 (if-not (assign values (first dplaces) d)
		      false
		      true)  
		  true)))))

(defn grid-values [grid]
  (zipmap squares grid))

(defn parse-grid [grid]
  (let [values (ref (into {} (for [s squares] [s digits])))]
    (if-not (all? (for [[s d] (grid-values grid) :when (in? d digits)]
		    (assign values s d)))
      false
      values)))

(defn display [values]
  (let [values-strs (for [s squares] (apply str (@values s)))
	rows (partition 9 values-strs)
	lines (for [r rows] (str/join " " (interpose-nth 3 "|" r)))]
    (doseq [line (interpose-nth 3 "---------------------" lines)]
      (println line))))

(defn search [values]
  (cond
   (false? values) false
   (all? (for [s squares] (= 1 (count (@values s))))) values
   :else (let [unfilled (for [s squares :when (> (count (@values s)) 1)] s)
	       s (apply min-key #(count (@values %)) unfilled)]
	   (some #(search (assign (copy values) s %)) (@values s)))))

(defn solve-grid [grid]
  (search (parse-grid grid)))

(defn solve-file [file]
  (with-open [rdr (reader file)]
    (let [lines (line-seq rdr)
	  grid (str/join lines)]
      (solve-grid grid))))
