module Main where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Table
import Control.Applicative
import Control.Monad
import Graphics.Vty.Attributes (Attr, defAttr)
import Graphics.Vty.Attributes.Color
import Graphics.Vty.Input.Events (Event(..), Key(..), Modifier(..), Button(..))
import Sudoku
import Data.Char (digitToInt, isDigit)
import Data.Text qualified as Text
import Data.Void
import GHC.Read (list)

-- These values can change as the user plays the game.
-- focus stores the currently-selected cell in the puzzle
-- puzzle stores the Puzzle the user is solving
-- setUpMode tracks whether the user is setting up or solving the puzzle
data AppState where
  AppState :: 
    { focus :: (LineIndex, LineIndex)
    , puzzle :: Puzzle
    , setUpMode :: Bool
    } -> AppState

-- A Brick UI widget to represent a single cell in the puzzle.
-- Surrounds the cell text with a border if it's selected, or one space
-- of padding if it's unselected
cellWidget :: Bool -> Cell -> Widget Void
cellWidget selected cell = 
  let
    cellText = Text.pack $ cellToString cell
    attr = 
      case cell of
        Guess _ -> "Guess"
        Given _ -> "Given"
        _ -> "Blank"
    baseWidget = withAttr (attrName attr) $
        txt cellText
  in 
    if selected then 
      border baseWidget
    else
      padAll 1 baseWidget


-- A Brick UI widget to represent an entire puzzle as a table of cells.
-- Puts border between each cell.
boardWidget :: AppState -> Widget Void
boardWidget st = 
  renderTable $ table $ 
  (liftA2 . liftA2) 
    cellWidget 
    (listofLists (fmap (\i -> i == focus st) allCoordinates))
    (puzzleToLists (puzzle st))
  
-- compute decreasing a LineIndex. Wraps back around to the top/other side if you're at the bottom/edge
indexMinus :: LineIndex -> LineIndex
indexMinus A = I
indexMinus B = A
indexMinus C = B
indexMinus D = C
indexMinus E = D
indexMinus F = E
indexMinus G = F
indexMinus H = G
indexMinus I = H
 
-- compute increasing a LineIndex. Wraps back around to the bottom/other side if you're at the top/edge
indexPlus :: LineIndex -> LineIndex
indexPlus A = B
indexPlus B = C
indexPlus C = D
indexPlus D = E
indexPlus E = F
indexPlus F = G
indexPlus G = H
indexPlus H = I
indexPlus I = A

-- When the user presses an arrow key, these up/down/left right functions
-- compute the next focus location.
up :: (LineIndex, LineIndex) -> (LineIndex, LineIndex)
up (r, c) = (indexPlus r, c)

down :: (LineIndex, LineIndex) -> (LineIndex, LineIndex)
down (r, c) = (indexMinus r, c)

right :: (LineIndex, LineIndex) -> (LineIndex, LineIndex)
right (r, c) = (r, indexPlus c) 

left :: (LineIndex, LineIndex) -> (LineIndex, LineIndex)
left (r, c) = (r, indexMinus c)

-- Handle a BrickEvent, which represents a user input or some other change in the
-- terminal state outside our application. 
-- Brick has two commands that we will use in the EventM Monad:
-- halt takes a final AppState and exits the UI thread
-- continue takes a next AppState and continues the UI thread.
handleEvent :: AppState -> BrickEvent Void Void -> EventM Void (Next (AppState))
handleEvent st event = 
  case event of
    -- The VtyEvent constructor with an EvKey argument indicates that the user
    -- has pressed a key on the keyboard. The empty list in the pattern
    -- indicates that no modifier keys (Shift/Ctrl/...) were being held down
    -- while the key was pressed.
    VtyEvent (EvKey key []) -> 
      case key of
        KLeft -> continue $ st {focus = left (focus st)}
        KRight -> continue $ st {focus = right (focus st)}
        KUp -> continue $ st {focus = up (focus st)}
        KDown -> continue $ st {focus = down (focus st)}
        KEsc -> halt st
        KChar x -> handleKey x st ""
        _ -> continue st
    VtyEvent (EvKey key [MShift]) ->
      case key of
        KChar x -> handleKey x st "note"
        _ -> continue st
    _ -> continue st

handleKey :: Char -> AppState -> String -> EventM Void (Next (AppState))
handleKey 'm' st _ = continue $ st {setUpMode = False}
handleKey x st n = 
  let 
    -- fromEnum returns the ASCII value of the char, have to subtract 49 to get correct Int
    key = (fromEnum x) - 48
  in
    if (key >= 1 && key <= 9) then
      if n == "note" then
        -- update the notepad
        -- need to implement the notepad
        continue $ st
      else
        -- update the puzzle
        if setUpMode st then
          continue $ st {puzzle = setGiven (puzzle st) key (focus st)} 
        else
          continue $ st {puzzle = setGuess (puzzle st) key (focus st)} 
    else
      -- some key besides 1-9 was pressed, no change to puzzle
      continue $ st

-- The attribute map for our application, which Brick uses to apply text styles
-- to our widgets.
gameAttrMap :: AttrMap
gameAttrMap = 
  attrMap
  (brightWhite `on` black) -- default scheme for names not listed below
  [ (attrName "Guess", fg green)
  , (attrName "Given", fg blue)
  ]

-- The Brick application definition, which is used to generate our main
-- function. appChooseCursor and appStartEvent are given "default" values that
-- just opt out of those Brick features. appDraw calls boardWidget to render
-- the UI, appHandleEvent calls handleEvent to respond to user inputs, and
-- gameAttrMap defines the text style of the UI.
app :: App (AppState) Void Void
app = 
  App
    { appDraw = \st -> [boardWidget st]
    , appChooseCursor = \_ _ -> Nothing
    , appHandleEvent = handleEvent
    , appStartEvent = pure
    , appAttrMap = \_ -> gameAttrMap
    }


testingAppState :: AppState
testingAppState = 
  AppState
    { focus = (A, A)
    , puzzle = samplePuzzle
    , setUpMode = False
    }

initialAppState :: AppState
initialAppState = 
  AppState
    { focus = (A, A)
    , puzzle = blankPuzzle
    , setUpMode = True
    }

main :: IO ()
main = do
  finalAppState <- defaultMain app testingAppState
  when 
    ((checkPuzzle (puzzle finalAppState)) &&
    (all (/=0) (map (getCellValue (puzzle finalAppState)) allCoordinates))) $
      putStrLn "Congratulations! You solved the Puzzle"
  putStrLn "final puzzle:"
  print (puzzle finalAppState)


-- This function converts a char input from the user to a LineIndex
-- It is used to validate user input in the promptPlayer function
-- It takes in a char and returns a Just LineIndex if the char is a valid LineIndex, or Nothing if not
charToLineIndex :: Char -> Maybe LineIndex 
charToLineIndex 'A' = Just A
charToLineIndex 'B' = Just B 
charToLineIndex 'C' = Just C
charToLineIndex 'D' = Just D
charToLineIndex 'E' = Just E
charToLineIndex 'F' = Just F
charToLineIndex 'G' = Just G
charToLineIndex 'H' = Just H
charToLineIndex 'I' = Just I
charToLineIndex _ = Nothing

-- This function determines if the user is allowed to change the location they want while they are in the current mode
-- It's main use is to prevent the player from overwriting or deleting Given values while they are in solve mode, since the plain text representation of the puzzle has no visual indication which values are Given or Guessed, it would be easy for the player to make this mistake.
-- It takes in:
  -- The Cell the user wants to change, and
  -- The mode currently in use (a string)
-- It returns True if the value in the cell and the current mode are matched
allowedAction :: Cell -> String -> Bool 
allowedAction Blank _ = True            -- A Blank value can be changed in either mode
allowedAction (Given x) "setup" = True  -- A Given value can only be changed in setup mode
allowedAction (Guess x) "solve" = True  -- A Guess value can only be changed in solve mode
allowedAction _ _ = False               -- Any other combination is not allowed

-- This function takes user input to either setup or solve the puzzle.
promptPlayer :: String -> Puzzle -> IO Puzzle
promptPlayer mode p = do
  putStrLn "Enter your coordinates and value:"
  line <- getLine
  case line of 
    ['(', row, ',', ' ', col, ')', ' ', x] -> 
      -- coordinates & value, like '(A, A) 7', were entered
      -- Test the coordinates entered for validity
      case (charToLineIndex row, charToLineIndex col) of
        (Just r, Just c) -> 
          -- The coordinates entered were valid
          -- Next test if the value x is a digit
          case isDigit x of
            True -> 
              -- The x value is a digit
              -- Next test if the value x is within the allowed range [1-9]
              -- Note that setGiven and setGuess both test the value of x and will not set the Cell if it's not in the range [1-9], so it's not strictly necessary to test x here. 
              -- However, testing here as well creates a better user experience in the case of invalid input, we can give the user a helpful prompt instead of just silently failing to update the board.
              case allowedValue (digitToInt x) of
                True -> 
                  -- The x value is in the allowed range [1-9]
                  -- Next test if the user is allowed to set the value at this location in this mode
                    case (allowedAction (p (r, c)) mode) of 
                      True -> 
                        -- The user is allowed to set the value at this location in this mode.
                        case mode of
                          "setup" -> 
                             -- in setup mode we don't let the user make mistakes.
                              -- we need to test if this entry is valid in these coordinates.
                              -- Again, setGiven does this testing already and will not set a Cell to an invalid value, but we test it here to provide a helpful prompt to the user instead of failing silently
                              case validEntry p (digitToInt x) (r, c) of
                                True -> 
                                  -- It is a valid entry in this cell
                                  --  setGiven will make a new Puzzle using this input and the value will be labeled as a Given
                                  do
                                    -- make the new puzzle
                                    let p1 = setGiven p (digitToInt x) (r, c)
                                    -- print the new puzzle
                                    print p1
                                    -- invoke promptPlayer again with the new puzzle
                                    promptPlayer mode p1

                                False -> 
                                  -- It is not a valid entry in this cell
                                  -- prompt user to try again
                                  do
                                    badInput "invalid"
                                    promptPlayer mode p 

                          _ -> do
                              -- solve mode
                              -- setGuess will make a new Puzzle using this input and the value will be labeled as a Guess
                              -- make the new puzzle
                              let p1 = setGuess p (digitToInt x) ((r, c))
                              -- print the new puzzle
                              print p1
                              -- invoke promptPlayer again with the new puzzle
                              promptPlayer mode p1

                      False -> do
                        -- the type of value at the coordinates doesn't match the mode we are in. This error message assumes the user is trying to overwrite a Given value in solve mode. Due to the program flow, that's really the only way this will be encountered.
                        badInput "mode"
                        promptPlayer mode p

                False -> 
                  -- The value x is not in the range [1-9]
                  -- prompt user to try again
                  do
                    badInput "number"
                    promptPlayer mode p 

            False -> 
            -- The x value is not a digit
            -- prompt user to try again
              do
                badInput "number"
                promptPlayer mode p 

        _ -> 
          -- At least one of the coordinates entered were not valid
          -- prompt user to try again
          do
            badInput "coordinates"
            promptPlayer mode p  
    
    ['d', 'e', 'l', 'e', 't', 'e', ' ', '(', row, ',', ' ', col, ')'] -> 
      -- delete command, like 'delete (A, A), was entered
      -- this pattern is so ugly, I will find a better way to do it later
      case (charToLineIndex row, charToLineIndex col) of
        (Just r, Just c) -> 
          -- two valid LineIndex were given 
          -- We need to test that the user is allowed to delete the value at this location in this mode
          case (allowedAction (p (r, c)) mode) of
            True -> do
              -- The user is allowed to delete the value at this location in this mode
              -- make the new puzzle
              let p1 = eraseCell p (r, c)
              -- print the new puzzle
              print p1
              -- invoke promptPlayer again with the new puzzle
              promptPlayer mode p1 
                
            False -> do
              -- the value at the coordinates doesn't match the mode we are in. This error message assumes the user is trying to delete a Given value in solve mode. Due to the program flow, that's really the only way this will be encountered.
              badInput "mode"
              promptPlayer mode p

        _ -> do
          -- one or both LineIndex values were invalid, prompt user to try again
          badInput "coordinates"
          promptPlayer mode p

    ['t', 'y', 'p', 'e', ' ', '(', row, ',', ' ', col, ')'] ->
      -- player wants to know if the coordinates were set in setup mode or solve mode
      -- test if the coordinates are valid
      case (charToLineIndex row, charToLineIndex col) of
        (Just r, Just c) -> do
          -- tell the user what kind of value is there
          case p (r, c) of
            Blank -> 
              putStrLn "That location is currently blank" 
            Given x -> 
              putStrLn "That location was set in setup mode" 
            Guess y -> 
              putStrLn "That location was set in solve mode"
          
          -- reinvoke promptPlayer
          promptPlayer mode p

        _ -> do
          badInput "coordinates"
          promptPlayer mode p

    "print" -> do
      -- player wants to see their puzzle
      print p
      promptPlayer mode p

    "check" -> 
      -- player wants to check their puzzle for errors
      case checkPuzzle p of
        True -> do
          putStrLn "All entries so far are valid"
          promptPlayer mode p
        False -> do
          let coords = findInvalidCoords p allCoordinates
          -- This string is kind of ugly when there are more than one error location. I can do this a better way with a helper function.
          putStrLn ("The value(s) at " ++ coords ++ "conflicts with other values in the row, cell, or box it belongs to")
          promptPlayer mode p
    
    "done" -> 
    -- finished
      case mode of 
        -- in setup mode, just return the puzzle
        "setup" -> pure p
        _ -> 
          -- in solve mode
          -- test if the puzzle is complete by testing if all entries in the puzzle are not blank (represented by 0)
          case all (/=0) (map (getCellValue p) allCoordinates) of
            True -> do
              -- puzzle is complete, test if it has errors
                case checkPuzzle p of
                  True -> do
                    -- puzzle has no errors
                    putStrLn "Congratulations, you solved the puzzle!"
                    -- return to main
                    pure p

                  False -> do
                    -- puzzle has errors, see if the player wants to try to continue solving it
                    putStrLn "The puzzle has errors, do you want to continue solving? y/n"
                    line <- getLine
                    case line of 
                      -- player wants to continue, reinvoke promptPlayer
                      "y" -> promptPlayer mode p  
                      -- player does not want to continue, return to main
                      "n" -> pure p
                      _ -> do
                        -- player didn't answer y or n
                        putStrLn "I didn't understand your answer"
                        -- reinvoke promptPlayer
                        promptPlayer mode p

            False -> do
              -- puzzle is incomplete, ask player if they want to continue solving
              putStrLn "The puzzle isn't finished, do you want to continue solving? y/n"
              line <- getLine
              case line of 
                -- player wants to continue, reinvoke promptPlayer
                "y" -> promptPlayer mode p
                -- player does not want to continue, return to main
                "n" ->  pure p
                _ -> do
                  -- player didn't answer y or n
                  putStrLn "I didn't understand your answer"
                  -- reinvoke promptPlayer
                  promptPlayer mode p
      

    "help" -> do
      -- user wants list of commands
      -- list the commands based on what mode we're in
      case mode of 
        "solve" -> listCommandsSolve
        _ -> listCommands

      -- reinvoke promptPlayer
      promptPlayer mode p
    
    _ -> do
    -- error entry
    -- this pattern catches numbers greater than 9. I need to add another pattern match to catch this better. (give a more helpful prompt)
      badInput "other"
      promptPlayer mode p

-- This function just tells the player what they did wrong when an error is encountered
badInput :: String -> IO ()
badInput error = do
  case error of
    "invalid" -> putStrLn "That value already exists in either that row, column, or box. Try again."
    "mode" -> putStrLn "That spot got it's value in the setup mode. You can't overwrite it in this mode."
    "number" -> putStrLn "You can only use numbers between 1 and 9. Try again."
    "coordinates" -> putStrLn "Puzzle coordinates must be between the letters A and I. Try again."
    -- this will be "other" but I have that flag turned on so I'm using the wildcard
    _ -> putStrLn "Invalid input, try again."
  pure ()

-- This function reminds the player what the valid command prompts are
listCommands :: IO ()
listCommands = do
  putStrLn "make an entry:                  (A, A) 7"
  putStrLn "delete an entry:                delete (A, A)"
  putStrLn "finished making entries:        done"
  putStrLn "print the puzzle:               print"
  putStrLn "check if cell contains a guess: type"
  pure ()

-- The solve mode has one more prompt than the setup mode, so this function calls listCommands to list the commands they both use and then prints the one extra command.
listCommandsSolve :: IO ()
listCommandsSolve = do
  listCommands
  putStrLn "check for errors:               check"
  pure ()

-- This function asks the player if they want to setup their own puzzle or use a sample puzzle.
-- In the future there will be an option to choose an easy, medium, or hard puzzle. To implement this I need to research the algorithm for generating a puzzle and then write the algorithm
getStarted :: IO Puzzle 
getStarted = do
  line <- getLine
  case line of 
    -- player wants to solve the sample puzzle. Return samplePuzzle defined in Sudoku.hs
    "sample" -> do
      -- print the puzzle so the player can see it
      print samplePuzzle
      -- return the puzzle
      pure samplePuzzle 

    "setup" -> do
      -- player wants to setup their own puzzle. Invoke promptPlayer in solve mode using the blankPuzzle defined in Sudoku.hs
      putStrLn "*** SETUP MODE ***"
      putStrLn "In setup mode, I will help you by making sure that you do not create an unsolvable puzzle."
      putStrLn "If you try to set a cell to a value that already exists in it's row, column, or box, I will ask you to try again."
      putStrLn "When you are finished setting up your puzzle, say 'done' (without quotes) to begin solving your puzzle."
      putStrLn "\n"
      promptPlayer "setup" blankPuzzle
    _ -> do
      -- player entered some other command, prompt them again.
      putStrLn "I didn't understand what you said, try again. Say 'setup' or 'sample' (without quotes)"
      getStarted

main2 :: IO ()
main2 = do
  -- Introduction
  putStrLn "Welcome to Sudoku!"
  putStrLn "\n"
  putStrLn "A Sudoku puzzle is a 9x9 grid of squares which are subdivided into 3x3 boxes."
  putStrLn "Each place in the puzzle can hold a value from 1 to 9"
  putStrLn "A value can only be used once in the row, column, or 3x3 box that it belongs to."
  putStrLn "Each row and column in the puzzle is labeled with the letters A-I, and"
  putStrLn "cells in the puzzle are identified with their (row, column) coordinates."
  putStrLn "For example, the coordinates (A, C) refer to the cell in the first row and the third column."
  putStrLn "\n"

  -- Instructions to set values in the puzzle
  putStrLn "To set the value of a cell in the puzzle, enter the coordinates and the value separated by a space."
  putStrLn "Here is an example:" 
  putStrLn "(A, A) 7"
  putStrLn "If you make a mistake, you can delete that space as follows:"
  putStrLn "delete (A, A)"
  putStrLn "To print your puzzle, say 'print' (without quotes)"
  putStrLn "If you need to see a list of commands, say 'help' (without quotes)"
  putStrLn "\n"
  

  -- find out how the player wants to get started 
  putStrLn "To get started, you can setup a new puzzle to solve, or solve a sample puzzle."
  putStrLn "Which would you like to try? Say 'setup' or 'sample' (without quotes)"

  p <- getStarted 
  
  -- Now the player is ready to solve the puzzle.
  -- Show some instructions about solving the puzzle.
  putStrLn "*** SOLVE MODE ***"
  putStrLn "Now your puzzle is ready to be solved. (Scroll up to see it's current state). "
  putStrLn "You can check your puzzle for mistakes by saying 'check' (without quotes)"
  putStrLn "You should also know that you can only change or delete entries you've made while solving the puzzle."
  putStrLn "You cannot change or delete entries that you made during the setup phase."
  putStrLn "You can check what mode a cell got it's value in by saying 'type' followed by a space and the coordinates, like:"
  putStrLn "type (A, A)"
  putStrLn "When you are done solving your puzzle, say 'done' (without quotes)"
  putStrLn "\n"

  -- Invoke promptPlayer to start solving the Puzzle
  p1 <- promptPlayer "solve" p

  -- The player said they are done solving the puzzle
  -- say goodbye
  putStrLn "Thank you for playing!"

  -- All done
  pure ()
    
