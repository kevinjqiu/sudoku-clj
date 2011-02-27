(ns sudoku.solver
  (:use [clojure.contrib.logging :only [spy info]]
        [clojure.contrib.string :only [join]]
        [sudoku :only [in?]]
        [sudoku.const :only [*squares* *peers* *digits* *units* *rows* *cols*]]))

(def *test-grid* "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......")

(def *solving* (ref true))

(declare assign)
(declare eliminate)
(declare render-grid)

(defn- create-init-grid []
  (into {} (map #(vector % *digits*) *squares*)))

(defn- grid-values [grid]
  "convert grid string into a dict of {square:char} with '.' representing empty cell"
  (zipmap *squares* grid))

(defn- eliminated [digits digit]
  "returns digits with digit eliminated"
  (clojure.string/replace-first digits digit ""))

(defn- try-assign-digit-in-unit [grid-ref current-unit digit]
  "assign digit to the one place available to it in unit,
  otherwise, return false"
  (println (str "try-assign-digit-in-unit: " (apply str current-unit) ", " digit))
  (let [places-for-digit (filter #(in? (get @grid-ref %) digit) current-unit)]
    (cond (empty? places-for-digit) (do (println "no place for digit") false) ;; Contradiction: no place for digit
          (= 1 (count places-for-digit))
            ;; assign it!
          (do (println "assign it")
            (assign grid-ref (first places-for-digit) digit))
          :else true)))

(defn- try-assign-digit-in-units [grid-ref units digit]
  (some true? (map #(try-assign-digit-in-unit grid-ref % digit) units)))

(defn- no-contradictions? [grid-ref square remaining-digits]
  (println remaining-digits)
  (cond 
    (empty? remaining-digits) (do (println "last digit removed" false)) ;; Contradiction: last digit removed
    (= 1 (count remaining-digits))
      ;; if a square is reduced only to a single value, 
      ;; eliminate that value from its peers.
      (every? true? (map #(eliminate grid-ref % remaining-digits) (get *peers* square)))
    :else
      true))

(defn- eliminate [grid-ref square digit]
  "Eliminate d from @grid-ref[square]; propagate when values or places <= 2.
  Return false if contradiction is detected. False otherwise"
  (println (str "eliminate[" square "," digit "]"))
  (let [digits (get @grid-ref square)]
    (if (not (in? digits digit)) true
      (let [new-values (eliminated digits digit)]
        (dosync (alter grid-ref #(assoc % square new-values)))
        (print (render-grid @grid-ref))
        (if @*solving* (read-line) "")
        ;(println (str "new-gridref" (get @grid-ref square)))
        (if (not (no-contradictions? grid-ref square new-values)) false
          ;; if the unit is reduced to only one place for value d, then put it there
          (try-assign-digit-in-units grid-ref (get *units* square) digit))))))

(defn- assign [grid-ref square digit]
  "Eliminate all the other values (except d) from grid[square]
  and propagate. Return false if a contradiction is detected"
  (println (str "assign[" square "," digit "]"))
  (let [digits-to-eliminate (eliminated (get @grid-ref square) digit)]
    (every? true? (map #(eliminate grid-ref square %) digits-to-eliminate))))

(defn- parse-grid [grid-str]
  "convert grid-str to a map of possible values,
  in the form of {square: digits},
  or False if a contradiction is detected."
  (let [grid-ref (ref (create-init-grid))
        values (grid-values grid-str)]
    ;; if digit is not in *digits*, it's a blank cell
    (if (every? true? (map #(or (not (in? *digits* (last %))) (assign grid-ref (first %) (last %))) values))
      (do (println "YAY!") grid-ref)
      (do (println "OH CRAP") false))))

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

(defn- render-grid [grid]
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

(defn- find-next-square [grid-ref]
  (let [candidates (filter #(< 1 (first %)) (map (fn [square] (let [candidates (get @grid-ref square) cnt (count candidates)] [cnt square])) *squares*))]
    (if (empty? candidates) 
      (throw (java.lang.Exception.))
      (last (apply min-key #(first %) candidates)))))

(defn- copy-map [a-map]
  (apply hash-map (interleave (keys a-map) (vals a-map))))

(defn- search [grid-ref]
  (cond (false? @grid-ref) (do (println "grid-ref=false") false)
        (every? true? (map #(= 1 (count (get @grid-ref %))) *squares*)) grid-ref ;; solved!
        :else
          ;; choose the unfilled square s with the fewest possibilities
          (let [s (find-next-square grid-ref)
                success (some (-> not false?) (map #(let [new-grid-ref (ref (copy-map @grid-ref))] (if (assign new-grid-ref s %) (search new-grid-ref) false)) (get @grid-ref s)))]
            (if (true? success) @grid-ref false))))

(defn solve [grid-str]
  (let [grid-ref (parse-grid grid-str)]
    (dosync (ref-set *solving* true))
    (search grid-ref)))

(println (render-grid @(parse-grid *test-grid*)))

;(try
;  (let [grid (solve *test-grid*)]
;    (println (render-grid grid)))
;
;  (catch Exception ex (println (-> ex .getMessage))))

