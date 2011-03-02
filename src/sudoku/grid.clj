(ns sudoku.grid
  (:use [sudoku :only [in?]]
        [clojure.set :only [union difference]]))

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
  (zipmap
    *squares*
    (map (fn [s] (filter #(in? % s) *unitlist*)) *squares*)))

(def *peers*
  (zipmap *squares*
  (map #(filter (fn [s] (not (= % s))) (apply concat (get *units* %))) *squares*))
)

;; getters
(defn peers [square]
  (get *peers* square))

(defn units [square]
  (get *units* square))

