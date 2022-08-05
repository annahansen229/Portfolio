# Introduction
This program implements a simple Sudoku puzzle.

A user can solve a medium-difficulty puzzle which is included in the file, or set up their own puzzle to solve. It includes a method to check if the current state of a puzzle is valid, and if not, displays the locations within the puzzle that are in conflict. The methodology is explained in comments within the code.

# Launching the App
To build and run the program, execute:
```
cabal run Sudoku
```

after the first play, you can play again without re-building the file. To do so, execute:
```
cabal exec Sudoku
```
# Interacting with the Puzzle
On startup, you need to say `sample` if you want to solve the built-in puzzle, or `setup` if you want to start from scratch.

If you setup your own puzzle, I help you out by preventing you from making conflicting entries in setup mode.

You can navigate around the puzzle using the arrow keys: `up`, `down`, `left`, and `right`. 

You can make entries in the puzzle using the number keys, `1`-`9`.

When you are done with setup mode, press `Enter` to switch to solve mode.

In solve mode, I do allow you to make mistakes. But, you can check for conflicting entries by pressing `c`. Any conflicts will be highlighted in red for you. When you make a new entry, any error indicators will be removed from the display.

In solve mode you cannot modify entries that were made in setup mode. These are displayed in blue to help you keep track of where you can make changes.

In either mode, you can erase an entry by pressing `Backspace`. (You cannot erase blue entries in solve mode)

When you are finished playing, press `Esc`.

# App Upgrades
Improvements I made to this program for Spring 2022 Quarter:
* **Implemented terminal user interface using the Brick library.**
  
  I borrowed extensively from the Minesweeper homework, and modified as needed. This was a big improvement in the user experience for the program because now the user doesn't have to enter manual coordinates to make entries on the puzzle. Using different colors on the puzzle also helped differentiate the given and guess values from each other. 

  I also wanted to visually differentiate the 3x3 boxes in the puzzle. I researched modifying the border weights of the appropriate locations and found that I could not do this using the `table` method, which comes with default borders but the only available modification is to turn them on and off, it was not an option to modify the style for the nth rows and columns. I could have done this by abandoning the `table` method and rendering borders on the individual `cellWidget`s, but I was constrained by time. 

  I attempted an alternate method by changing the background color of every other 3x3 box, however my `Shade` attribute is not being applied to the cells and I was constrained by time from troubleshooting this issue.

  This upgrade also lays the groundwork for my notepad feature.

* **Refactored the Cell data type to use a record type.**

  This was the first step required to begin implementing the notepad feature. Nearly all of my functions had to be refactored to work with the new definition. 
  
  Along the way I made other changes implementing features of the Functor and Applicative typeclasses which made my code a lot more concise. In some cases features that relied upon as many as three functions chained together were rewritten into only one or two lines. 

* **Began implementing the notepad feature**

  I defined the data type for my notepad feature, which I chose to define as a 9-tuple of Bool. I made this choice because I want the values in the notepad to be displayed in fixed locations, like the examples below:

  A full notepad:
  ```
  1 2 3
  4 5 6
  7 8 9
  ```
  A notepad containing 1, 6, and 8:
  ```
  1    
      6
    8  
  ```
  A notepad containing only 9:
  ```
       
       
      9 
  ```

  And I thought this would require a lot of checking for each value if I used some flexible-sized container, whereas using the 9-tuple of Bool I can just check the value of the corresponding member of the tuple.

  I defined functions for setting and erasing values in the notepad, and printing a notepad. 
  
  The next steps in the process for implementing the notepad feature will be extending the tui commands to support making entries in the notepad, modifying the `cellWidget` to display the notepad, and extending the `setGuess` function so that it:
  
  1. deletes the cell's notepad when an entry is made, and 
  
  2. manages the notepads in the corresponding row, column, and box by removing the value the cell was set to so that the user doesn't have to delete them manually. 

Some future goals to expand the application are still:
* puzzle writing algorithm
* puzzle solving algorithm
* ability to set up a puzzle from a file, instead of by hand
