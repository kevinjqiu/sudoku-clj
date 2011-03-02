(ns sudoku
  (:use [sudoku.grid :only [render-grid]]
        [sudoku.solver :only [solve]]))

(def *test-grid* "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......")

(let [grid (solve *test-grid*)]
  (println (render-grid @grid)))

