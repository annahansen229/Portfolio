module Sudoku (module Sudoku) where

import Control.Applicative
import Data.Bits
import Data.Maybe

-- A cell in the Sudoku puzzle is either blank, or contains a value. If it contains a value, the value is either given to start with or has been guessed by the player. A cell can also contain a notepad instead of a value. The implementation details enforce that a cell cannot have both a notepad and a value at the same time.
data Cell where
    Cell :: 
        { given :: Bool
        , value :: Maybe Int
        , notepad :: Maybe Notepad
        } -> Cell

blankCell :: Cell
blankCell = 
    Cell 
        { given = False
        , value = Nothing
        , notepad = Nothing 
        }

-- A notepad allows the user to make notes of what they think the value of the cell might be
-- The first member of the tuple represents the value 1, and so on.
type Notepad = (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)

-- in this example, the user would have made a note that 1 or 2 could be value of this location
exampleNotepad :: Notepad
exampleNotepad = (True, True, False, True, False, False, False, False, False)

blankNotepad :: Notepad
blankNotepad = (False, False, False, False, False, False, False, False, False)

-- set the indicated location of the Notepad to True, retaining the existing values of the other locations
-- an out of bounds value returns the Notepad unchanged
setNote :: Notepad -> Int -> Notepad
setNote (n1, n2, n3, n4, n5, n6, n7, n8, n9) x =
    case x of
        1 -> (True, n2, n3, n4, n5, n6, n7, n8, n9)
        2 -> (n1, True, n3, n4, n5, n6, n7, n8, n9)
        3 -> (n1, n2, True, n4, n5, n6, n7, n8, n9)
        4 -> (n1, n2, n3, True, n5, n6, n7, n8, n9)
        5 -> (n1, n2, n3, n4, True, n6, n7, n8, n9)
        6 -> (n1, n2, n3, n4, n5, True, n7, n8, n9)
        7 -> (n1, n2, n3, n4, n5, n6, True, n8, n9)
        8 -> (n1, n2, n3, n4, n5, n6, n7, True, n9)
        9 -> (n1, n2, n3, n4, n5, n6, n7, n8, True)
        _ -> (n1, n2, n3, n4, n5, n6, n7, n8, n9)

eraseNote :: Notepad -> Int -> Notepad
eraseNote (n1, n2, n3, n4, n5, n6, n7, n8, n9) x =
    case x of
        1 -> (False, n2, n3, n4, n5, n6, n7, n8, n9)
        2 -> (n1, False, n3, n4, n5, n6, n7, n8, n9)
        3 -> (n1, n2, False, n4, n5, n6, n7, n8, n9)
        4 -> (n1, n2, n3, False, n5, n6, n7, n8, n9)
        5 -> (n1, n2, n3, n4, False, n6, n7, n8, n9)
        6 -> (n1, n2, n3, n4, n5, False, n7, n8, n9)
        7 -> (n1, n2, n3, n4, n5, n6, False, n8, n9)
        8 -> (n1, n2, n3, n4, n5, n6, n7, False, n9)
        9 -> (n1, n2, n3, n4, n5, n6, n7, n8, False)
        _ -> (n1, n2, n3, n4, n5, n6, n7, n8, n9)

printNotepad :: Notepad -> String
printNotepad (n1, n2, n3, n4, n5, n6, n7, n8, n9) = 
    (if n1 then "1" else "_") ++
    (if n2 then "2" else "_") ++
    (if n3 then "3" else "_") ++
    "\n" ++
    (if n4 then "4" else "_") ++
    (if n5 then "5" else "_") ++
    (if n6 then "6" else "_") ++
    "\n" ++
    (if n7 then "7" else "_") ++
    (if n8 then "8" else "_") ++
    (if n9 then "9" else "_")

-- a string representation of a cell's value 
prettyCell :: Cell -> String
prettyCell cell = 
    case (value cell) of
        Just x -> show x
        Nothing -> " "

instance Show Cell where show = prettyCell

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

-- Each space in the Puzzle, represented by a pair of LineIndex, returns a Cell
type Puzzle = (LineIndex, LineIndex) -> Cell

-- Here is a list representation of all Puzzle coordinates
allCoordinates :: [(LineIndex, LineIndex)]
allCoordinates = liftA2 (,) allLineIndex allLineIndex

-- Here is how to print a Cell
-- This is a helper function for printing a row of a Puzzle
-- The string representation of the Cell's value is concatenated with the pipe character in order to print the Puzzle in grid format
printCell :: Puzzle -> (LineIndex, LineIndex) -> String
printCell p i = 
    show (p i) ++ " | "

-- Here is how to print a row in a Puzzle.
-- This is a helper function for printing a Puzzle
-- The string representation of each of the Cells are concatenated together
printRow :: Puzzle -> LineIndex -> String
printRow p r = 
    concat $ fmap (\c -> show (p (r, c)) ++ " | ") allLineIndex
-- # come back here and make sure this works. If yes delete printCell2

-- Here is how to print a whole Puzzle using the function that I defined above to print a row
-- Each row is led by it's LineIndex and separated from the next row by a line of dashes to create a labeled grid 
printPuzzle :: Puzzle -> String
printPuzzle p = 
    "  | A | B | C | D | E | F | G | H | I |" ++
    "\n---------------------------------------" ++
    "\nA | " ++ printRow p A ++
    "\n---------------------------------------" ++
    "\nB | " ++ printRow p B ++
    "\n---------------------------------------" ++
    "\nC | " ++ printRow p C ++
    "\n---------------------------------------" ++
    "\nD | " ++ printRow p D ++
    "\n---------------------------------------" ++
    "\nE | " ++ printRow p E ++
    "\n---------------------------------------" ++
    "\nF | " ++ printRow p F ++
    "\n---------------------------------------" ++
    "\nG | " ++ printRow p G ++
    "\n---------------------------------------" ++
    "\nH | " ++ printRow p H ++
    "\n---------------------------------------" ++
    "\nI | " ++ printRow p I ++ 
    "\n---------------------------------------\n" 

instance Show Puzzle where show = printPuzzle

-- represents a puzzle as a list of lists of Cells
puzzleToLists :: Puzzle -> [[Cell]]
puzzleToLists p = fmap (\r -> concat $ fmap (\c -> [p (r, c)]) allLineIndex) allLineIndex

-- breaks a list up into sublists of length 9
-- used to build the boardWidget
listofLists :: [a] -> [[a]]
listofLists xs = 
    case splitAt 9 $ xs of
        (ys, []) -> [ys]
        (ys, yys) -> ys : listofLists yys

-- Here is a blankPuzzle, this will be the starting point for each new Puzzle
blankPuzzle :: Puzzle
blankPuzzle = (\(r, c) -> blankCell)

-- This function will set a Given value of a Cell in the Puzzle.
-- It takes in:
    -- the Puzzle being updated, 
    -- the value to update the Cell to, and
    -- the coordinates of the Cell being set, 
-- It returns an updated copy of the puzzle
-- setGiven doesn't allow the user to make an invalid entry in the puzzle.
setGiven :: Puzzle -> Int -> (LineIndex, LineIndex) -> Puzzle
setGiven p x i =
    (\j -> 
        case ((i == j), (validEntry p x i)) of
            (True, True) -> 
                -- correct location, valid entry. Update cell.
                (p j) {given = True, value = Just x}

            _ ->
                -- incorrect location or invalid entry. Return same cell. 
                (p j))
    
-- This function will set a Guess value of a Cell in the Puzzle
-- It takes in:
    -- the Puzzle being updated, 
    -- the value to update the Cell to, and
    -- the coordinates of the Cell being set, 
-- It returns an updated copy of the puzzle
-- Note that unlike setGiven, setGuess only verifies that the guess is a valid value. It does not verify that the guess is valid in it's position in the puzzle. This allows the user to make a mistake while they are working on the puzzle. 
setGuess :: Puzzle -> Int -> (LineIndex, LineIndex) -> Puzzle
setGuess p x i = 
    if (given (p i)) then
        -- Cannot set guess value on given cell. Return puzzle with no change
        p
    else
        (\j -> 
            if (i == j) then
                -- correct location, update cell
                -- setting a value clears the notepad
                (p j) {value = Just x, notepad = Nothing}
            else
                -- incorrect location, return same cell
                (p j))

-- This function removes a value from the puzzle
-- It takes in:
    -- The puzzle being updated
    -- The coordinates of the cell being erased
-- It returns an updated copy of the puzzle
eraseCell :: Puzzle -> (LineIndex, LineIndex) -> Puzzle
eraseCell p i = 
    (\j -> 
        if (i == j) then
            (p j) {value = Nothing}
        else 
            (p j))

-- This function checks if a value is valid in the row and column where it will exist
-- It takes in:
    -- A Puzzle
    -- The proposed value, and
    -- The location in the Puzzle
-- It returns True if the value doesn't exist elsewhere in the same row or column, or False if it does
-- This is a helper function for determining if a value is valid in a Puzzle.
validInRowAndColumn :: Puzzle -> Int -> (LineIndex, LineIndex) -> Bool
validInRowAndColumn p x (r, c) = 
    all (== True) $ 
    fmap 
        (\(i, j) -> 
            case xor (i == r) (j == c) of
                True -> 
                    -- We are in either the same row or the same column
                    case value (p (i, j)) of
                        Nothing -> True
                        Just y -> 
                            -- This will return false if the same value is found in the row or column of the coordinates
                            not (x == y)
                False -> 
                    -- We are either at the same location, or the wrong row or the wrong column
                    -- We do not check against these values
                    True)
        allCoordinates

-- In a sudoku puzzle values belong not only to their row and column but also to a 3x3 box which is a subset of the whole puzzle.
-- There are nine such boxes in a sudoku puzzle.
box1 :: [(LineIndex, LineIndex)]
box1 = [(A, A), (A, B), (A, C),
        (B, A), (B, B), (B, C), 
        (C, A), (C, B), (C, C)]

box2 :: [(LineIndex, LineIndex)]
box2 = [(A, D), (A, E), (A, F), 
        (B, D), (B, E), (B, F), 
        (C, D), (C, E), (C, F)]

box3 :: [(LineIndex, LineIndex)]
box3 = [(A, G), (A, H), (A, I), 
        (B, G), (B, H), (B, I), 
        (C, G), (C, H), (C, I)]

box4 :: [(LineIndex, LineIndex)]
box4 = [(D, A), (D, B), (D, C), 
        (E, A), (E, B), (E, C), 
        (F, A), (F, B), (F, C)]

box5 :: [(LineIndex, LineIndex)]
box5 = [(D, D), (D, E), (D, F), 
        (E, D), (E, E), (E, F), 
        (F, D), (F, E), (F, F)]

box6 :: [(LineIndex, LineIndex)]
box6 = [(D, G), (D, H), (D, I), 
        (E, G), (E, H), (E, I), 
        (F, G), (F, H), (F, I)]

box7 :: [(LineIndex, LineIndex)]
box7 = [(G, A), (G, B), (G, C),
        (H, A), (H, B), (H, C),
        (I, A), (I, B), (I, C)]

box8 :: [(LineIndex, LineIndex)]
box8 = [(G, D), (G, E), (G, F),
        (H, D), (H, E), (H, F),
        (I, D), (I, E), (I, F)]

box9 :: [(LineIndex, LineIndex)]
box9 = [(G, G), (G, H), (G, I),
        (H, G), (H, H), (H, I),
        (I, G), (I, H), (I, I)]

-- Now that each box has been defined I can put them together into a list
-- This will be used in the following function which checks whether a value is valid in the box it belongs to.
boxes :: [[(LineIndex, LineIndex)]]
boxes = [box1, box2, box3, box4, box5, box6, box7, box8, box9]

-- This function checks if a value exists in a 3x3 box
-- It takes in:
    -- The Puzzle being updated
    -- The value to update the cell to
    -- a 3x3 box which is a list of pairs of coordinates
-- If the value is in the 3x3 box it returns True
-- This is a helper function for determining if a value at a location is valid in it's box. Once we know the box it belongs to, we need to know if the value already exists in the box.
valueInBox :: Puzzle -> Int -> (LineIndex, LineIndex) -> [(LineIndex, LineIndex)] -> Bool
valueInBox p x (r, c) box = 
    any (== True) $ fmap 
        (\(i, j) -> 
            if (i == r) && (j == c) then
                -- don't check against self
                False
            else
                case (value (p (i, j))) of 
                    Nothing -> False
                    Just y -> x == y)
        box

-- This function checks if the value the user wants to set the Cell to already exists in the 3x3 box it belongs to
-- First it finds which box the Cell belongs to
-- Then it determines if the value already exists in that box using the valueInBox function defined above
-- It takes in:
    -- the Puzzle being updated, 
    -- the value to update the Cell to, and
    -- the coordinates of the Cell being set
-- It returns True if the value does not already exist in the 3x3 box
-- This is a helper function for determining if an entry is valid in the Puzzle
validInBox :: Puzzle -> Int -> (LineIndex, LineIndex) -> Bool
validInBox p x i = 
    all (== False) $ fmap 
        (\box -> 
            if (elem i box) then
                -- found the box the coordinates belong to
                valueInBox p x i box
            else
                -- not the right box to check
                False) 
        boxes
    

-- This function determines whether a value is valid in the Puzzle
-- It takes in:
    -- The puzzle being updated
    -- The value to update the cell to
    -- The coordinates of the Cell being updated
-- If the value is valid in the Puzzle it returns True.
-- In order for a value to be valid, it must:
    -- not be repeated in the row or column, and
    -- not be repeated in the 3x3 box where it is located
validEntry :: Puzzle -> Int -> (LineIndex, LineIndex) -> Bool
validEntry p x i = 
    (validInRowAndColumn p x i) &&
    (validInBox p x i)

-- This function checks if there are any errors in a Puzzle by fmapping the validEntry function over all locations in the puzzle.
-- It takes in a Puzzle and returns True if all entries are valid.
checkPuzzle :: Puzzle -> Bool
checkPuzzle p = 
    all (== True) $ fmap 
    (\i -> case value (p i) of
        Nothing -> True
        Just x -> validEntry p x i)
    allCoordinates

-- This function returns a list of any locations that have conflicting entries in the puzzle
findInvalidCoords :: Puzzle -> [(LineIndex, LineIndex)]
findInvalidCoords p =
    catMaybes $ fmap 
        (\i -> 
            if given (p i) then
                Nothing
            else
                case value (p i) of
                    Nothing -> Nothing
                    Just x -> 
                        if validEntry p x i then
                            Nothing
                        else
                            Just i) 
        allCoordinates

-- Here is a samplePuzzle I can use for testing
samplePuzzleRecord :: Puzzle 
samplePuzzleRecord (A, A) = Cell {given = True, value = Just 4, notepad = Nothing} 
samplePuzzleRecord (A, B) = Cell {given = True, value = Just 9, notepad = Nothing} 
samplePuzzleRecord (A, C) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (A, D) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (A, E) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (A, F) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (A, G) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (A, H) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (A, I) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (B, A) = Cell {given = True, value = Just 7, notepad = Nothing} 
samplePuzzleRecord (B, B) = Cell {given = True, value = Just 5, notepad = Nothing} 
samplePuzzleRecord (B, C) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (B, D) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (B, E) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (B, F) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (B, G) = Cell {given = True, value = Just 8, notepad = Nothing} 
samplePuzzleRecord (B, H) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (B, I) = Cell {given = True, value = Just 2, notepad = Nothing} 
samplePuzzleRecord (C, A) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (C, B) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (C, C) = Cell {given = True, value = Just 8, notepad = Nothing} 
samplePuzzleRecord (C, E) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (C, D) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (C, F) = Cell {given = True, value = Just 5, notepad = Nothing} 
samplePuzzleRecord (C, G) = Cell {given = True, value = Just 9, notepad = Nothing} 
samplePuzzleRecord (C, H) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (C, I) = Cell {given = True, value = Just 3, notepad = Nothing} 
samplePuzzleRecord (D, A) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (D, B) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (D, C) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (D, D) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (D, E) = Cell {given = True, value = Just 7, notepad = Nothing} 
samplePuzzleRecord (D, F) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (D, G) = Cell {given = True, value = Just 1, notepad = Nothing} 
samplePuzzleRecord (D, H) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (D, I) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (E, A) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (E, B) = Cell {given = True, value = Just 8, notepad = Nothing} 
samplePuzzleRecord (E, C) = Cell {given = True, value = Just 5, notepad = Nothing} 
samplePuzzleRecord (E, D) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (E, E) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (E, F) = Cell {given = True, value = Just 9, notepad = Nothing} 
samplePuzzleRecord (E, G) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (E, H) = Cell {given = True, value = Just 3, notepad = Nothing} 
samplePuzzleRecord (E, I) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (F, A) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (F, B) = Cell {given = True, value = Just 7, notepad = Nothing} 
samplePuzzleRecord (F, C) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (F, D) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (F, E) = Cell {given = True, value = Just 6, notepad = Nothing} 
samplePuzzleRecord (F, F) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (F, G) = Cell {given = True, value = Just 2, notepad = Nothing} 
samplePuzzleRecord (F, H) = Cell {given = True, value = Just 8, notepad = Nothing} 
samplePuzzleRecord (F, I) = Cell {given = True, value = Just 9, notepad = Nothing} 
samplePuzzleRecord (G, A) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (G, B) = Cell {given = True, value = Just 2, notepad = Nothing} 
samplePuzzleRecord (G, C) = Cell {given = True, value = Just 7, notepad = Nothing} 
samplePuzzleRecord (G, D) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (G, E) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (G, F) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (G, G) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (G, H) = Cell {given = True, value = Just 9, notepad = Nothing} 
samplePuzzleRecord (G, I) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (H, A) = Cell {given = True, value = Just 6, notepad = Nothing} 
samplePuzzleRecord (H, B) = Cell {given = True, value = Just 4, notepad = Nothing} 
samplePuzzleRecord (H, C) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (H, D) = Cell {given = True, value = Just 2, notepad = Nothing} 
samplePuzzleRecord (H, E) = Cell {given = True, value = Just 5, notepad = Nothing} 
samplePuzzleRecord (H, F) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (H, G) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (H, H) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (H, I) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (I, A) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (I, B) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (I, C) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (I, D) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (I, E) = Cell {given = True, value = Just 8, notepad = Nothing} 
samplePuzzleRecord (I, F) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (I, G) = Cell {given = True, value = Just 7, notepad = Nothing} 
samplePuzzleRecord (I, H) = Cell {given = False, value = Nothing, notepad = Nothing} 
samplePuzzleRecord (I, I) = Cell {given = False, value = Nothing, notepad = Nothing} 