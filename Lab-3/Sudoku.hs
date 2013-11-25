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