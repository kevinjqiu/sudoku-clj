Sudoku solver
=============
This is an adaptation of the sudoku solving algorithm from Peter Norvig's [article](norvig.com/sudoku.html) which was originally in Python. 

Run
===
The project is managed using [cake](https://github.com/ninjudd/cake). Although I imagine [leiningen](https://github.com/technomancy/leiningen) would work too.

Modify src/sudoku.clj to use a different initial configuration.
Then:
`cake run src/sudoku.clj`

Todo
====
+ Investigate using Clojure's trampoline function to facilitate more effective mutual recursive calls. Trampoline doesn't grow the stack.
+ Allow command-line arguments
+ Benchmark -- currently the implementation is slower than the Python version. (WHY?)
