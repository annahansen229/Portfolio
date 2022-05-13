module Sudoku2 where

-- A sudoku puzzle is comprised of 9 rows and columns
data LineIndex where
    A :: LineIndex
    B :: LineIndex
    C :: LineIndex
    D :: LineIndex
    E :: LineIndex
    F :: LineIndex
    G :: LineIndex
    H :: LineIndex
    I :: LineIndex
    deriving (Ord, Eq, Show)

-- Here is a list representation of LineIndex
allLineIndex :: [LineIndex]
allLineIndex = [A, B, C, D, E, F, G, H, I]

-- Here is a list representation of all Puzzle coordinates
allCoordinates :: [(LineIndex, LineIndex)]
allCoordinates = liftA2 (,) allLineIndex allLineIndex

-- Each space in the Puzzle, represented by a pair of LineIndex, returns a Cell
type Puzzle = (LineIndex, LineIndex) -> Cell

-- Each Cell in a Sudoku puzzle consists of an entry and a notepad
data Cell where
    Cell :: 
        {entry :: Entry, notepad :: Notepad} -> Cell

-- An entry in the Sudoku puzzle was either Given to start with or Guessed by the player.
data Entry where
    Given :: Int -> Entry
    Guess :: Int -> Entry

-- All that matters in the string representation of an Entry is the value that it carries.
entryToString :: Entry -> String
entryToString _ x = show x

instance Show Entry where show = entryToString

-- A notepad in the Sudoku puzzle lets the user keep track of what the solution to a Cell might be
type Notepad = (LineIndex, LineIndex) -> Maybe Int

-- All Notepads start blank
blankNotepad :: Notepad
blankNotepad = (\(r, c) -> Nothing)

-- All Puzzles start blank
blankPuzzle :: Puzzle
blankPuzzle = (\(r, c) -> Cell {entry = Nothing, notepad = blankNotepad})

-- This function updates the notepad. It takes in:
    -- The update mode (erase or add)
    -- The value being updated
    -- The notepad being updated
-- It returns the updated notepad.
updateNotepad :: String -> Int -> Notepad -> Notepad
updateNotepad mode x n = 
    let (i, j) = 
        -- the number being updated corresponds to a location in the notepad
        case x of
            1 -> (A, A)
            2 -> (A, B)
            3 -> (A, C)
            4 -> (B, A)
            5 -> (B, B)
            6 -> (B, C)
            7 -> (C, A)
            8 -> (C, B)
            9 -> (C, C)
    in
        (\(r,c) -> 
            if (i, j) == (r, c) then
                -- At the correct location in the notepad make the update
                case mode of
                    "add" -> Just x
                    "erase" -> Nothing
            else
                -- At any other location in the notepad return the value that was already there
                n (r, c))
