(ns utils)

(defn all? [coll]
  (not-any? false? coll))

(defn in? [x coll]
  (some #{x} coll))

(defn copy [reference]
  (ref @reference))

(defn interpose-nth [n sep coll]
  (apply concat (interpose [sep] (partition n coll))))
