(ns sudoku.const
  (:use [clojure.set :only [union difference]]))

(defn- in? [seq elem]
  "Surprisingly, clojure doesn't provide a standard function
  for such a frequently used case?
  contains? tests whether an element can be `get` from a coll"
  (some #(= % elem) seq))

(defn- x [as, bs]
  "Cross product of A x B"
  (for [a as b bs] (str a b)))

(def *digits* "123456789")
(def *rows* "ABCDEFGHI")
(def *cols* "123456789")
(def *squares* (x *rows* *cols*))
(def *unitlist* 
  (concat
    (for [c *cols*] (x *rows* (str c)))
    (for [r *rows*] (x (str r) *cols*))
    (for [rs '("ABC", "DEF", "GHI") cs '("123" "456" "789")] (x rs cs))))
                
; XXX: better idiom?
(defn- collect [map tuple]
  (let [key (first tuple)
        val (last tuple)]
    (assoc map key (conj (get map key #{}) val))))

(def *units*
  (reduce
    collect
    {}
    (for [s *squares*
          units (filter #(in? % s) *unitlist*)]
      [s units])))

(def *peers*
  (reduce
    collect
    {}
    (for [s *squares*
          peers (set (difference (reduce #(union %1 (set %2)) #{} (get *units* "A1")) (set s)))]
      [s peers])))

