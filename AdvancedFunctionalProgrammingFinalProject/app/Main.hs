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
import Data.List
import Data.Maybe
import Data.Text qualified as Text
import Data.Void
import Debug.Trace
import GHC.Read (list)

-- These values can change as the user plays the game.
-- focus stores the currently-selected cell in the puzzle
-- puzzle stores the Puzzle the user is solving
-- setUpMode tracks whether the user is setting up or solving the puzzle
-- errors is a list of indeces with conflicting entries
data AppState where
  AppState :: 
    { focus :: (LineIndex, LineIndex)
    , puzzle :: Puzzle
    , setUpMode :: Bool
    , errors :: [(LineIndex, LineIndex)]
    } -> AppState

-- A Brick UI widget to represent a single cell in the puzzle.
-- Surrounds the cell text with a border if it's selected, or one space
-- of padding if it's unselected
-- cellWidget :: Bool -> Cell -> Bool -> Widget Void
cellWidget :: Bool -> Cell -> Bool -> (LineIndex, LineIndex) -> Widget Void
cellWidget selected cell error index = 
  let
    -- Text.pack :: String -> Text
    cellText = Text.pack $ show cell
    -- this designates what color the text will be (mapped in GameAttrMap)
    attr = 
      if (given cell) then
        "Given"
      else
        if error then "Error" else "Guess"

    baseWidget = 
      if any (elem index) [box2, box4, box6, box8] then
        -- change the background color for alternating boxes
        withAttr (attrName attr <> attrName "Shade") $ txt cellText
      else
        withAttr (attrName attr) $ txt cellText
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
  (zipWith4 . zipWith4) 
    cellWidget 
    (listofLists (fmap (\i -> i == focus st) allCoordinates))
    (puzzleToLists (puzzle st))
    (listofLists (fmap (\i -> elem i (errors st)) allCoordinates))
    (listofLists allCoordinates)
  
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
up (r, c) = (indexMinus r, c)

down :: (LineIndex, LineIndex) -> (LineIndex, LineIndex)
down (r, c) = (indexPlus r, c)

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
        KEnter -> continue $ st {setUpMode = False}
        KBS -> handleErase st
        KChar 'c' -> continue $ st {errors = findInvalidCoords (puzzle st)}
        KChar x -> handleKey x st ""
        _ -> continue st
    VtyEvent (EvKey key [MShift]) ->
      case key of
        KChar x -> handleKey x st "note"
        _ -> continue st
    _ -> continue st

handleKey :: Char -> AppState -> String -> EventM Void (Next (AppState))
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
          -- reset error indicators after a new entry is made
          continue $ st {puzzle = setGuess (puzzle st) key (focus st), errors = []} 
    else
      -- some key besides 1-9 was pressed, no change to puzzle
      continue $ st

handleErase :: AppState -> EventM Void (Next (AppState))
handleErase st = 
  if ((given ((puzzle st) (focus st))) && (not (setUpMode st))) then
    -- This location is a given value and we are not in setup mode. Erasing is not allowed.
    continue $ st
  else
    continue $ st {puzzle = eraseCell (puzzle st) (focus st)}

-- The attribute map for our application, which Brick uses to apply text styles
-- to our widgets.
gameAttrMap :: AttrMap
gameAttrMap = 
  attrMap
  (brightWhite `on` black) -- default scheme for names not listed below
  [ (attrName "Guess", fg green)
  , (attrName "Given", fg blue)
  , (attrName "Error", fg red)
  , (attrName "Shade", bg red) -- this one is not getting applied
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
    , puzzle = samplePuzzleRecord
    , setUpMode = False
    , errors = []
    }

initialAppState :: AppState
initialAppState = 
  AppState
    { focus = (A, A)
    , puzzle = blankPuzzle
    , setUpMode = True
    , errors = []
    }

promptPlayerApp :: IO AppState
promptPlayerApp = do
  putStrLn "Do you want to solve a sample puzzle or setup your own puzzle? Say 'sample' or 'setup' (without quotes)"
  str <- getLine
  case str of
    "sample" -> do
      putStrLn "To erase an entry, navigate to it's location, then press Backspace."
      putStrLn "Note: you cannot erase the blue starting values."
      putStrLn "To check your puzzle for errors, press the 'c' key."
      putStrLn "Note: error indicators will be cleared after a new entry is made."
      putStrLn "When you are finished solving your puzzle, press Escape to exit the game."
      putStrLn "Press Enter now to proceed to solve your puzzle"
      getLine 
      pure testingAppState
    "setup" -> do
      putStrLn "When you are finished entering your initial values, press Enter to switch to solve mode."
      putStrLn "To erase an entry, navigate to it's location, then press Backspace."
      putStrLn "Note: you cannot erase blue values after you switch to solve mode."
      putStrLn "To check your puzzle for errors in solve mode, press the 'c' key."
      putStrLn "Note: error indicators will be cleared after a new entry is made."
      putStrLn "When you are finished solving your puzzle, press Escape to exit the game."
      putStrLn "Press Enter now to proceed to setup your puzzle"
      getLine 
      pure initialAppState
    _ -> promptPlayerApp

main :: IO ()
main = do
  selection <- promptPlayerApp
  finalAppState <- defaultMain app selection
  when (all (/= Nothing) (map (\i -> value ((puzzle finalAppState) i)) allCoordinates)) $
      if checkPuzzle (puzzle finalAppState) then
        putStrLn "Congratulations! You solved the Puzzle!"
      else
        putStrLn "Better luck next time!"

  putStrLn "final puzzle:"
  print (puzzle finalAppState)

