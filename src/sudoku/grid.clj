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

(defn- centre [content width]
  (if (>= (count content) width)
    content
    (let [num-spaces (- width (count content))
          spaces-before (int (/ num-spaces 2))
          spaces-after (- num-spaces spaces-before)]
      (str 
        (apply str (repeat spaces-before " ")) 
        content 
        (apply str (repeat spaces-after " "))))))

(defn render-grid [grid]
  (let [width (+ 1 (apply max (map #(count (get grid %)) *squares*)))
        line (clojure.string/join "+" (repeat 3 (apply str (repeat (* width 3) \-))))]
    (apply str (map 
      (fn [row] 
        (str
          (apply 
            str 
            (apply str (map 
              (fn [col] 
                (str 
                  (centre (get grid (str row col)) width)
                  (if (contains? #{\3 \6} col) "|" "")))
              *cols*)))
          "\n"
          (if (contains? #{\C \F} row) (str line "\n") "")))
      *rows*))))

(defn create-init-grid []
  (zipmap *squares* (repeat (count *squares*) *digits*)))

(defn grid-values [grid-str]
  "convert grid string into a dict of {square:char} with '.' representing empty cell"
  (zipmap *squares* grid-str))
