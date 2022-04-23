module Sudoku where

import Control.Applicative
import Data.Bits

-- trying to define a new sudoku value
type Value = (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)

-- Depending on which place is true, return the corresponding numeric value.
getValue :: Value -> Maybe Int 
getValue (True, False, False, False, False, False, False, False, False) = Just 1
getValue (False, True, False, False, False, False, False, False, False) = Just 2
getValue (False, False, True, False, False, False, False, False, False) = Just 3
getValue (False, False, False, True, False, False, False, False, False) = Just 4
getValue (False, False, False, False, True, False, False, False, False) = Just 5
getValue (False, False, False, False, False, True, False, False, False) = Just 6
getValue (False, False, False, False, False, False, True, False, False) = Just 7
getValue (False, False, False, False, False, False, False, True, False) = Just 8
getValue (False, False, False, False, False, False, False, False, True) = Just 9
getValue _ = Nothing

-- Depending on which value is being recorded, set that place to True and the rest to False.
setValue :: Int -> Maybe Value
setValue 1 = Just (True, False, False, False, False, False, False, False, False)  
setValue 2 = Just (False, True, False, False, False, False, False, False, False)  
setValue 3 = Just (False, False, True, False, False, False, False, False, False)    
setValue 4 = Just (False, False, False, True, False, False, False, False, False)    
setValue 5 = Just (False, False, False, False, True, False, False, False, False)    
setValue 6 = Just (False, False, False, False, False, True, False, False, False)    
setValue 7 = Just (False, False, False, False, False, False, True, False, False)    
setValue 8 = Just (False, False, False, False, False, False, False, True, False)    
setValue 9 = Just (False, False, False, False, False, False, False, False, True)    
setValue _ = Nothing

-- A Notepad is a special form of a sudoku value
type Notepad = Value

-- An Entry in a sudoku puzzle is either given by the puzzle, or guessed by the user.
data Entry where 
    Given :: Value -> Entry
    Guess :: Value -> Entry

-- A Cell in a sudoku puzzle contains an Entry (which could be blank) and a Notepad.
type Cell = (Maybe Entry, Notepad)

-- A blank cell has no entry and a blank notepad
blankCell :: Cell
blankCell = (Nothing, blankNotepad)

-- a blank Notepad is all false
blankNotepad :: Notepad
blankNotepad = (False, False, False, False, False, False, False, False, False)

-- adding to the Notepad sets that place to True and retains the other values as they were
addNotepad :: Int -> Notepad -> Notepad
addNotepad 1 (n1, n2, n3, n4, n5, n6, n7, n8, n9) = (True, n2, n3, n4, n5, n6, n7, n8, n9)
addNotepad 2 (n1, n2, n3, n4, n5, n6, n7, n8, n9) = (n1, True, n3, n4, n5, n6, n7, n8, n9)
addNotepad 3 (n1, n2, n3, n4, n5, n6, n7, n8, n9) = (n1, n2, True, n4, n5, n6, n7, n8, n9)
addNotepad 4 (n1, n2, n3, n4, n5, n6, n7, n8, n9) = (n1, n2, n3, True, n5, n6, n7, n8, n9)
addNotepad 5 (n1, n2, n3, n4, n5, n6, n7, n8, n9) = (n1, n2, n3, n4, True, n6, n7, n8, n9)
addNotepad 6 (n1, n2, n3, n4, n5, n6, n7, n8, n9) = (n1, n2, n3, n4, n5, True, n7, n8, n9)
addNotepad 7 (n1, n2, n3, n4, n5, n6, n7, n8, n9) = (n1, n2, n3, n4, n5, n6, True, n8, n9)
addNotepad 8 (n1, n2, n3, n4, n5, n6, n7, n8, n9) = (n1, n2, n3, n4, n5, n6, n7, True, n9)
addNotepad 9 (n1, n2, n3, n4, n5, n6, n7, n8, n9) = (n1, n2, n3, n4, n5, n6, n7, n8, True)
-- Any other value returns the notepad unchanged
addNotepad _ (n1, n2, n3, n4, n5, n6, n7, n8, n9) = (n1, n2, n3, n4, n5, n6, n7, n8, n9)

eraseNotepad :: Int -> Notepad -> Notepad
eraseNotepad 1 (n1, n2, n3, n4, n5, n6, n7, n8, n9) = (False, n2, n3, n4, n5, n6, n7, n8, n9)
eraseNotepad 2 (n1, n2, n3, n4, n5, n6, n7, n8, n9) = (n1, False, n3, n4, n5, n6, n7, n8, n9)
eraseNotepad 3 (n1, n2, n3, n4, n5, n6, n7, n8, n9) = (n1, n2, False, n4, n5, n6, n7, n8, n9)
eraseNotepad 4 (n1, n2, n3, n4, n5, n6, n7, n8, n9) = (n1, n2, n3, False, n5, n6, n7, n8, n9)
eraseNotepad 5 (n1, n2, n3, n4, n5, n6, n7, n8, n9) = (n1, n2, n3, n4, False, n6, n7, n8, n9)
eraseNotepad 6 (n1, n2, n3, n4, n5, n6, n7, n8, n9) = (n1, n2, n3, n4, n5, False, n7, n8, n9)
eraseNotepad 7 (n1, n2, n3, n4, n5, n6, n7, n8, n9) = (n1, n2, n3, n4, n5, n6, False, n8, n9)
eraseNotepad 8 (n1, n2, n3, n4, n5, n6, n7, n8, n9) = (n1, n2, n3, n4, n5, n6, n7, False, n9)
eraseNotepad 9 (n1, n2, n3, n4, n5, n6, n7, n8, n9) = (n1, n2, n3, n4, n5, n6, n7, n8, False)
-- Any other value returns the notepad unchanged
eraseNotepad _ (n1, n2, n3, n4, n5, n6, n7, n8, n9) = (n1, n2, n3, n4, n5, n6, n7, n8, n9)

-- Here is how to print a Cell value
-- For now I am not printing the notepad
cellToString :: Cell -> String
cellToString (e, n) = 
    case e of 
        Nothing -> " "
        Just (Guess v) -> 
            case getValue v of
                Nothing -> " "
                Just x -> show x
        Just (Given v) -> 
            case getValue v of
                Nothing -> " "
                Just x -> show x

instance Show Cell where show = cellToString

-- This function gets the value out of a cell so it can be compared with other values
-- It takes in:
    -- a Puzzle, and
    -- a set of coordinates
    -- (which together return a Cell in the puzzle)
-- It returns the value carried in the Cell
getCellValue :: Puzzle -> (LineIndex, LineIndex) -> Maybe Int
getCellValue p (r, c) = 
    -- access the cell
    case p (r, c) of
        (e, n) -> 
            -- access the entry
            case e of 
                Nothing -> Nothing
                Just (Guess v) -> getValue v
                Just (Given v) -> getValue v

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
printCell p (r, c) = 
    show (p (r, c)) ++ " | "

-- Here is how to print a row in a Puzzle.
-- This is a helper function for printing a Puzzle
-- The string representation of each of the Cells are concatenated together
printRow :: Puzzle -> LineIndex -> [LineIndex] -> String
-- end of list, return empty string
printRow p r [] = ""
-- concatenate this Cell's string representation with the rest of the list
printRow p r (c : cs) = printCell p (r, c) ++ printRow p r cs

-- Here is how to print a whole Puzzle using the function that I defined above to print a row
-- Each row is led by it's LineIndex and separated from the next row by a line of dashes to create a labeled grid 
printPuzzle :: Puzzle -> String
printPuzzle p = 
    "  | A | B | C | D | E | F | G | H | I |" ++
    "\n---------------------------------------" ++
    "\nA | " ++ printRow p A allLineIndex ++
    "\n---------------------------------------" ++
    "\nB | " ++ printRow p B allLineIndex ++
    "\n---------------------------------------" ++
    "\nC | " ++ printRow p C allLineIndex ++
    "\n---------------------------------------" ++
    "\nD | " ++ printRow p D allLineIndex ++
    "\n---------------------------------------" ++
    "\nE | " ++ printRow p E allLineIndex ++
    "\n---------------------------------------" ++
    "\nF | " ++ printRow p F allLineIndex ++
    "\n---------------------------------------" ++
    "\nG | " ++ printRow p G allLineIndex ++
    "\n---------------------------------------" ++
    "\nH | " ++ printRow p H allLineIndex ++
    "\n---------------------------------------" ++
    "\nI | " ++ printRow p I allLineIndex ++ 
    "\n---------------------------------------\n" 

instance Show Puzzle where show = printPuzzle

-- Here is a blankPuzzle, this will be the starting point for each new Puzzle
blankPuzzle :: Puzzle
blankPuzzle = (\(r, c) -> blankCell)

-- This function checks whether a value is allowed to be used in a Puzzle
-- The values in a sudoku puzzle must be in the range [1-9]
-- It takes in the value the player wants to use
-- It returns True if the value is in the range [1-9]
allowedValue :: Int -> Bool
allowedValue x = 1 <= x && x <= 9

-- This function will set a Given value of a Cell in the Puzzle.
-- It takes in:
    -- the Puzzle being updated, 
    -- the value to update the Cell to, 
    -- the coordinates of the Cell being set, and
    -- the type of the value
-- It returns an updated copy of the puzzle
-- setGiven doesn't allow the user to make an invalid entry in the puzzle.
setGiven :: Puzzle -> Int -> (LineIndex, LineIndex) -> Puzzle
setGiven p x (i, j) = 
    if validEntry p x (i, j) then
        -- input x is valid in Cell (i, j)
        -- update & return puzzle
        updatePuzzle p x (i, j) Given
    else
        -- input x is not valid in Cell (i, j)
        -- return the input puzzle
        p

-- This function will set a Guess value of a Cell in the Puzzle
-- It takes in:
    -- the Puzzle being updated, 
    -- the value to update the Cell to, 
    -- the coordinates of the Cell being set, and
    -- the type of the value
-- It returns an updated copy of the puzzle
-- Note that unlike setGiven, setGuess only verifies that the guess is a valid value. It does not verify that the guess is valid in it's position in the puzzle. This allows the user to make a mistake while they are working on the puzzle. 
setGuess :: Puzzle -> Int -> (LineIndex, LineIndex) -> Puzzle
setGuess p x (i, j) = 
    case p (i, j) of
        -- (maybe Entry, notepad)
        (e, n) -> 
            case e of 
                Just (Given v) -> 
                    -- A Given value cannot be over-written with a Guess value
                    -- return the input puzzle
                    p
                _ -> 
                    -- Both a Nothing value and a Guess value can be over-written with a Guess value
                    -- update & return the puzzle
                    updatePuzzle p x (i, j) Guess

-- This function removes a value from the puzzle
-- It takes in:
    -- The puzzle being updated
    -- The coordinates of the cell being erased
-- It returns an updated copy of the puzzle
eraseCell :: Puzzle -> (LineIndex, LineIndex) -> Puzzle
eraseCell p (i, j) =
    case p (i, j) of
        -- (maybe entry, notepad)
        (e, n) -> 
            case e of
                Nothing -> 
                    -- the cell is already blank, just return the input puzzle
                    p
                _ -> 
                    -- Something was there, but it doesn't matter what because it's being deleted anyway.
                    -- update the puzzle (0 and Guess are thrown away by updatePuzzle - they don't matter)
                    updatePuzzle p 0 (i, j) Guess
                    

-- This function sets a Cell in a Puzzle to a value
-- It takes in:
    -- The puzzle being updated (Puzzle)
    -- The value being updated (Int)
    -- The coordinates of the Cell being updated, (LineIndex, LineIndex) and 
    -- The type of Cell being set (Value -> Entry)
-- It returns an updated copy of the puzzle
updatePuzzle :: Puzzle -> Int -> (LineIndex, LineIndex) -> (Value -> Entry) -> Puzzle
updatePuzzle p x (i, j) g = 
    -- access the cell
    case p (i, j) of
        -- (maybe entry, notepad)
        (e, n) -> 
            case x of 
                0 -> 
                    -- if updatePuzzle is called with x = 0 we are deleting an entry to the puzzle. 
                    -- The input constructor and the value 0 are essentially discarded in this case.
                    -- This lambda function takes in a pair of LineIndex and returns a Cell. In other words, it is a Puzzle.
                    (\(r, c) ->
                        if (i, j) == (r, c) then
                            -- if (r, c) is the coordinates we want to update, return a cell with a blank entry and the existing cell's notepad
                            (Nothing, n)
                        else
                            -- (r, c) is not the coordinates we want to update, return the Cell at (r, c) in the input puzzle
                            p (r, c)
                    )
                _ ->
                    -- if updatePuzzle is called with x = any other value then we do the same as above but we do use the input constructor and value.
                    (\(r, c) ->
                        if (i, j) == (r, c) then
                            let v = setValue x in
                                case v of
                                    Nothing -> 
                                        -- if setValue returned Nothing, the input value was invalid. Return the cell as it was.
                                        (e, n)
                                    Just v' -> 
                                        (Just (g v'), n)
                        else
                            p (r, c)
                    )

-- This function checks if a value is valid in the row and column where it will exist
-- It takes in:
    -- A Puzzle
    -- The proposed value
    -- The location in the Puzzle, and
    -- The list of all Puzzle coordinates
-- It returns True if the value doesn't exist elsewhere in the same row or column, or False if it does
-- This is a helper function for determining if a value is valid in a Puzzle.
validInRowAndColumn :: Puzzle -> Int -> (LineIndex, LineIndex) -> [(LineIndex, LineIndex)] -> Bool
-- got to the end of the puzzle without finding a conflict, this entry must be valid
validInRowAndColumn p x (r, c) [] = True
validInRowAndColumn p x (r, c) ((i, j) : list) = 
    -- I use xor here because I need i == r (meaning we are in the right row) or j == c (meaning we are in the right column)
    -- but I do not want i == r and j == c (meaning we are at the same cell that we are checking)
    case xor (i == r) (j == c) of
        True -> 
            -- we are in the right row or the right column
            -- we need to test if the value at (i, j) is equal to x
            case getCellValue p (i, j) of
                Nothing ->
                    -- there is no value at (i, j), x is a valid entry
                    True 
                Just y ->
                    case x == y of
                        True -> 
                            -- the value at (i, j) is equal to x, x is not a valid entry
                            False 
                        False -> 
                            -- the value at (i, j) is not equal to x, check the rest of the list
                            validInRowAndColumn p x (r, c) list
        False -> 
            -- it is neither the right row or column, or it is the same cell, skip it and check the rest of the list
            validInRowAndColumn p x (r, c) list

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

-- This function checks if a cell is in a 3x3 box
-- It takes in:
    -- a cell represented by a pair of coordinates
    -- a 3x3 box which is a list of pairs of coordinates
-- If the cell is in the box it returns True
-- This is a helper function for determining if a value at a location is valid in it's box. To do that, we first need to know which box a location belongs to.
cellInBox :: (LineIndex, LineIndex) -> [(LineIndex, LineIndex)] -> Bool
-- base case, the coordinates cannot exist in an empty list
cellInBox (r, c) [] = False
cellInBox (r, c) ((i, j) : ijs) = 
    if (r, c) == (i, j) then
        -- if the coordinates match this element of the list, return true
        True
    else
        -- the coordinates do not match this element of the list, check the rest of the list
        cellInBox (r, c) ijs

-- This function checks if a value exists in a 3x3 box
-- It takes in:
    -- The Puzzle being updated
    -- The value to update the cell to
    -- a 3x3 box which is a list of pairs of coordinates
-- If the value is in the 3x3 box it returns True
-- This is a helper function for determining if a value at a location is valid in it's box. Once we know the box it belongs to, we need to know if the value already exists in the box.
valueInBox :: Puzzle -> Int -> (LineIndex, LineIndex) -> [(LineIndex, LineIndex)] -> Bool
-- base case, the value cannot exist in an empty list
valueInBox p x (r, c) [] = False
valueInBox p x (r, c) ((i, j) : ijs) = 
    -- I use xor here because I need i == r (meaning we are in the right row) or j == c (meaning we are in the right column)
    -- but I do not want i == r and j == c (meaning we are at the same cell that we are checking)
    case xor (i == r) (j == c) of
        True -> 
            case getCellValue p (i, j) of
                Nothing -> 
                    -- this cell is empty so the value doesn't match it. Check the rest of the box.
                    valueInBox p x (r, c) ijs 
                Just v ->
                    if x == v then
                        -- the value matches the value of this cell in the box, return true
                        True
                    else
                        -- the value doesn't match the value of this cell in the box, check the rest of the box
                        valueInBox p x (r, c) ijs 

        False -> 
            -- it is neither the right row or column, or it is the same cell, skip it and check the rest of the list
            valueInBox p x (r, c) ijs 

-- This function checks if the value the user wants to set the Cell to already exists in the 3x3 box it belongs to
-- First it finds which box the Cell belongs to using the cellInBox function defined above
-- Then it determines if the value already exists in that box using the valueInBox function defined above
-- It takes in:
    -- the Puzzle being updated, 
    -- the value to update the Cell to, 
    -- the coordinates of the Cell being set, and
    -- the list of 3x3 boxes
-- It returns True if the value does not already exist in the 3x3 box
-- This is a helper function for determining if an entry is valid in the Puzzle
validInBox :: Puzzle -> Int -> (LineIndex, LineIndex) -> [[(LineIndex, LineIndex)]]-> Bool
-- base case empty list of boxes (should never hit this case if we did there is something wrong with our code)
validInBox p x (r, c) [] = False
validInBox p x (r, c) (b : bs) = 
    if cellInBox (r, c) b then
        -- the cell being set is in this box
        -- check if the value being set is in this box and return the negated result of checking
        -- (if the value is in the box already then the value is not valid to add to the box)
        not (valueInBox p x (r, c) b)
    else
        -- the cell being set is not in this box
        -- check the rest of the list of boxes
        validInBox p x (r, c) bs      

-- This function determines whether a value is valid in the Puzzle
-- It takes in:
    -- The puzzle being updated
    -- The value to update the cell to
    -- The coordinates of the Cell being updated
-- If the value is valid in the Puzzle it returns True.
-- In order for a value to be valid, it must:
    -- be in the range [1-9],
    -- not be repeated in the row or column, and
    -- not be repeated in the 3x3 box where it is located
validEntry :: Puzzle -> Int -> (LineIndex, LineIndex) -> Bool
validEntry p x (r, c) = 
    (allowedValue x) && 
    (validInRowAndColumn p x (r, c) allCoordinates) &&
    (validInBox p x (r, c) boxes)

-- This function if the value in a specific Cell is valid in the Puzzle
-- It takes in:
    -- A Puzzle and
    -- A location in the puzzle 
-- It returns True if the entry is valid and False if not
-- Note, this function is not used to check if it's valid to *set* a cell to a value, it checks if the value already in a cell is valid.
checkCell :: Puzzle -> (LineIndex, LineIndex) -> Bool 
checkCell p (r, c) = 
    let v = getCellValue p (r, c) in
        case v of 
            Nothing -> 
                -- don't need to check blanks
                True 
            Just v' -> 
                -- check Guess and Given
                validEntry p v' (r, c)

-- This function checks if the values existing at a list of Puzzle coordinates in a Puzzle are valid by mapping the checkCell function over a list of coordinates
-- It takes in: 
    -- A puzzle and 
    -- A list of coordinates 
-- It returns a list of Bool that contains the True or False result from checkCell for each location in the Puzzle
checkCoords :: Puzzle -> [(LineIndex, LineIndex)] -> [Bool]
checkCoords p list = map (checkCell p) list

-- This function checks if there are any errors in a Puzzle by taking the list returned from checkCoords and determining if all entries in the list are True.
-- It takes in a Puzzle and returns True if all entries are valid.
checkPuzzle :: Puzzle -> Bool
checkPuzzle p = all (== True) (checkCoords p allCoordinates)

-- This function finds and returns a string representation of a list of the coordinates of any invalid Guess entries encountered in a Puzzle
findInvalidCoords :: Puzzle -> [(LineIndex, LineIndex)] -> String
-- base case empty list, return an empty string
findInvalidCoords p [] = ""
findInvalidCoords p ((r, c) : list) = 
    -- test what type of Value the cell has
    case p (r, c) of
        (Guess y) -> 
            -- in a Guess cell, check if the value is valid
            case checkCell p (r, c) of
                -- the value at (r, c) is valid, check the rest of the list
                True -> findInvalidCoords p list
                -- the value at (r, c) isn't valid, return the string representation of the coordinates and check the rest of the list
                False -> "(" ++ show r ++ ", " ++ show c ++ ") " ++ findInvalidCoords p list
        _ -> 
            -- don't need to check Blank or Given cells
            findInvalidCoords p list



-- Here is a samplePuzzle I can use for testing
samplePuzzle :: Puzzle 
samplePuzzle (A, A) = Given 4
samplePuzzle (A, B) = Given 9
samplePuzzle (A, C) = Blank
samplePuzzle (A, D) = Blank
samplePuzzle (A, E) = Blank
samplePuzzle (A, F) = Blank
samplePuzzle (A, G) = Blank
samplePuzzle (A, H) = Blank
samplePuzzle (A, I) = Blank
samplePuzzle (B, A) = Given 7
samplePuzzle (B, B) = Given 5
samplePuzzle (B, C) = Blank
samplePuzzle (B, D) = Blank
samplePuzzle (B, E) = Blank
samplePuzzle (B, F) = Blank
samplePuzzle (B, G) = Given 8
samplePuzzle (B, H) = Blank
samplePuzzle (B, I) = Given 2
samplePuzzle (C, A) = Blank
samplePuzzle (C, B) = Blank
samplePuzzle (C, C) = Given 8
samplePuzzle (C, D) = Blank
samplePuzzle (C, E) = Blank
samplePuzzle (C, F) = Given 5
samplePuzzle (C, G) = Given 9
samplePuzzle (C, H) = Blank
samplePuzzle (C, I) = Given 3
samplePuzzle (D, A) = Blank
samplePuzzle (D, B) = Blank
samplePuzzle (D, C) = Blank
samplePuzzle (D, D) = Blank
samplePuzzle (D, E) = Given 7
samplePuzzle (D, F) = Blank
samplePuzzle (D, G) = Given 1
samplePuzzle (D, H) = Blank
samplePuzzle (D, I) = Blank
samplePuzzle (E, A) = Blank
samplePuzzle (E, B) = Given 8
samplePuzzle (E, C) = Given 5
samplePuzzle (E, D) = Blank
samplePuzzle (E, E) = Blank
samplePuzzle (E, F) = Given 9
samplePuzzle (E, G) = Blank
samplePuzzle (E, H) = Given 3
samplePuzzle (E, I) = Blank
samplePuzzle (F, A) = Blank
samplePuzzle (F, B) = Given 7
samplePuzzle (F, C) = Blank
samplePuzzle (F, D) = Blank
samplePuzzle (F, E) = Given 6
samplePuzzle (F, F) = Blank
samplePuzzle (F, G) = Given 2
samplePuzzle (F, H) = Given 8
samplePuzzle (F, I) = Given 9
samplePuzzle (G, A) = Blank
samplePuzzle (G, B) = Given 2
samplePuzzle (G, C) = Given 7
samplePuzzle (G, D) = Blank
samplePuzzle (G, E) = Blank
samplePuzzle (G, F) = Blank
samplePuzzle (G, G) = Blank
samplePuzzle (G, H) = Given 9
samplePuzzle (G, I) = Blank
samplePuzzle (H, A) = Given 6
samplePuzzle (H, B) = Given 4
samplePuzzle (H, C) = Blank
samplePuzzle (H, D) = Given 2
samplePuzzle (H, E) = Given 5
samplePuzzle (H, F) = Blank
samplePuzzle (H, G) = Blank
samplePuzzle (H, H) = Blank
samplePuzzle (H, I) = Blank
samplePuzzle (I, A) = Blank
samplePuzzle (I, B) = Blank
samplePuzzle (I, C) = Blank
samplePuzzle (I, D) = Blank
samplePuzzle (I, E) = Given 8
samplePuzzle (I, F) = Blank
samplePuzzle (I, G) = Given 7
samplePuzzle (I, H) = Blank
samplePuzzle (I, I) = Blank