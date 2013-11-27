module Sudoku where

import Test.QuickCheck
import Data.Maybe
import Data.Char
import Data.List

{-
   Lab Assignment 3
   Anna Averianova & Tobias Deekens, 2013
-}

-------------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- creates a sudoku with only blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- checks if sud is really a valid representation of a sudoku puzzle
isSudoku :: Sudoku -> Bool
isSudoku s = rowCount == 9 && all isValidRow rows'
  where
    rows'     = rows s
    rowCount  = length rows'

isValidRow :: [Maybe Int] -> Bool
isValidRow r = all (\ x -> case x of
                      Nothing -> True
                      Just x  -> x `elem` [1..9]) r

-- checks if a sud has no blanks
isSolved :: Sudoku -> Bool
isSolved s = isSudoku s && all isRowSolved rows'
  where
    rows' = rows s
    isRowSolved = all $ not . isNothing

-------------------------------------------------------------------------

-- prints a representation of a sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku s = putStr (unlines (map printRow rows'))
  where
    rows' = rows s

-- helper printing a list of sud cells on the screen
printRow :: [Maybe Int] -> String
printRow = foldr ((++) . printCell) ""

-- helper printing a sud cell on the screen
printCell :: Maybe Int -> String
printCell Nothing   = "."
printCell (Just c)  = show c

-- reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku fp = do
                  contents <- readFile fp
                  let sud = parseSudoku contents
                  if isSudoku sud
                    then return ( sud )
                    else error "File containing invalid sud."

-- parses a sud from a string
parseSudoku :: String -> Sudoku
parseSudoku s = Sudoku (map parseRow (lines s))

-- parses a sud row from a string
parseRow :: String -> [Maybe Int]
parseRow = map parseCell

-- parses a sud cell from a char
parseCell :: Char -> Maybe Int
parseCell c | c == '.'  = Nothing
            | otherwise = Just (ord c - ord '0')

-------------------------------------------------------------------------

-- generates an arbitrary sud cell
cell :: Gen (Maybe Int)
cell = frequency
         [(9, return Nothing),
          (1, do c <- choose (1,9); return (Just c))]

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-- property indicating if a sud is valid
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku

-------------------------------------------------------------------------

type Block = [Maybe Int]

-- checks if a block is valid following the sud rules
isOkayBlock :: Block -> Bool
isOkayBlock b = length withoutNothing == length (nub withoutNothing)
  where
    withoutNothing = filter isJust b

-- returns rows, columns and blocks of a sud
blocks :: Sudoku -> [Block]
blocks s = rows' ++ columns' ++ blocks'
  where
    rows'    = rows s
    columns' = transpose rows'
    blocks'  = rowWalker rows'

-- helper function walking the sud's rows three at a time
rowWalker :: [[Maybe Int]] -> [[Maybe Int]]
rowWalker []  = []
rowWalker r   = columnWalker (take 3 r) ++ rowWalker (drop 3 r)

-- helper function returning a list of blocks given three rows
columnWalker :: [[Maybe Int]] -> [[Maybe Int]]
columnWalker (x:y:z:ws)
  | null x = []
  | otherwise = ((take 3 x) ++ (take 3 y) ++ (take 3 z)) :
                  columnWalker ((drop 3 x) : (drop 3 y) : (drop 3 z) : ws)

-- validates if a sudoku is valid (not solved)
isOkay :: Sudoku -> Bool
isOkay s = all isOkayBlock (blocks s)

-- property validating the amount of blocks and their sizes
prop_blocks :: Sudoku -> Bool
prop_blocks s = length blocks' == 27 && all (\b -> length b == 9) blocks'
  where
    blocks' = blocks s

-------------------------------------------------------------------------

type Pos = (Int,Int)

-- given a sud returns a list of the positions in the sud that are still blank
blanks :: Sudoku -> [Pos]
blanks s = concat [blanksHelper (r!!idx) idx | idx <- [0..8]]
  where r = rows s

-- helper function returning positions of empty fields within a row
blanksHelper :: [Maybe Int] -> Int -> [Pos]
blanksHelper r idx = zip rs idxs
  where
    idxs  = elemIndices Nothing r
    rs    = replicate 9 idx

-- property checking that all cells in the blanks list are actually blanks
prop_blanks :: Sudoku -> Bool
prop_blanks s = all (\ x -> isNothing (r !! (fst x) !! (snd x))) blanks'
  where
    blanks' = blanks s
    r       = rows s

-- given a list, and a tuple containing an index in the list and a new value
-- updates the given list with the new value at the given index
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) l (idx, t) | ( (length l) -1 ) == idx = (fst $ splitAt idx l) ++ [t]
                 | otherwise = (fst chopped) ++ [t] ++ (tail $ snd chopped)
  where
    chopped = splitAt idx l

prop_replace :: Eq a => [a] -> (Int,a) -> Property
prop_replace l (idx, t) = not (null l) ==>
                          idx >= 0 && idx < length l ==>
                            length l == length rl
                            && idx `elem` elemIndices t rl
                            && rl!!idx == t
  where rl = l !!= (idx, t)

-- given a Sudoku, a position, and a new cell value, updates the given
-- sud at the given position with the new value
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update s p c = Sudoku ( r !!= (rIdx, r!!rIdx !!= (cIdx, c)) )
  where
    r    = rows s
    rIdx = fst p
    cIdx = snd p

candidates :: Sudoku -> Pos -> [Int]
candidates s (x, y) = [1..9] \\ [fromJust x | x <- (filter isJust fll)]
  where
    bs  = blocks s
    rc  = bs!!x ++ bs!!(9+y)
    b   = bs!!( 17 + nthBlock (x `div` 3, y `div` 3) )
    fll = (rc ++ b)

-- Helper function returning the block number of a given position
nthBlock :: Pos -> Int
nthBlock (x, y) = (x * 3) + (y + 1)

-------------------------------------------------------------------------

-- Solves a valid sud with okay blocks
solve :: Sudoku -> Maybe Sudoku
solve s | not $ isSudoku s || not (all isOkayBlock blo)  = Nothing
        | otherwise                                      = solve' s bla
  where
    blo = blocks s
    bla = blanks s

-- Solve helper filling in all the blanks calling solveCell
solve' :: Sudoku -> [Pos] -> Maybe Sudoku
solve' s []     = Just s
solve' s (b:bs) = solveCell s b cs
  where
    cs = candidates s b

-- Solves a cell at a position with a candidate
solveCell :: Sudoku -> Pos -> [Int] -> Maybe Sudoku
solveCell s p cs | null cs                  = Nothing
                 | isOkay ps && isSolved ps = Just ps
                 | isNothing pps            = solveCell s p (tail cs)
                 | otherwise                = pps
  where
    ps  = update s p (Just (head cs))
    pps = solve ps

-- reading the Sudoku from the given file, solving it, and printing the answer
readAndSolve :: FilePath -> IO ()
readAndSolve fp = do
                    sud <- readSudoku fp
                    printSudoku (fromJust (solve sud))

isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf s uns = undefined

-------------------------------------------------------------------------

-- Example Sudoku
example :: Sudoku
example =
   Sudoku
     [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
     , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
     , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
     , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
     , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
     , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
     , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
     , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
     , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
     ]