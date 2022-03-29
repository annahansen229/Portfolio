This program implements a simple Sudoku puzzle.

A user can solve a medium-difficulty puzzle which is included in the file, or set up their own puzzle to solve. It includes a method to check if the current state of a puzzle is valid, and if not, displays the locations within the puzzle that are in conflict. The methodology is explained in comments within the code.

To run the program, execute 
```
cabal build
```

then
```
cabal exec Sudoku
```

The program explains to you how to work with the puzzle as you go along. 

There are many opportunities to expand and improve the program. Some future goals include:
* puzzle writing algorithm
* puzzle solving algorithm
* ability to set up a puzzle using a complete list of coordinates and values entered as a string or read from a file, instead of one-by-one which is very tedious and error prone
