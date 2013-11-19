module Sudoku where

import Test.QuickCheck
import Data.Maybe
import Data.Char
import Data.List

-------------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku s = rowCount == 9 && all isValidRow rows'
  where
    rows'     = rows s
    rowCount  = length rows'
    isValidRow :: [Maybe Int] -> Bool
    isValidRow r
      = length (filter
          (\ x -> isNothing x || fromJust x >= 1 && fromJust x <= 9) r) == 9

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved s = isSudoku s && all isRowSolved rows'
  where
    rows' = rows s
    isRowSolved = all isNothing -- as field boundaries are checked by isSudoku

-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku s = putStr (unlines (map printRow rows'))
  where
    rows' = rows s
    printRow :: [Maybe Int] -> String
    printRow []     = ""
    printRow (x:xs) = printCell x ++ printRow xs
    printCell :: Maybe Int -> String
    printCell Nothing = "."
    printCell cell    = show $ fromJust cell

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku fp = do
                  contents <- readFile fp
                  return ( parseSudoku contents )
  where
    parseSudoku :: String -> Sudoku
    parseSudoku s = Sudoku (map parseRow (lines s))
    parseRow :: String -> [Maybe Int]
    parseRow = map parseCell
    parseCell :: Char -> Maybe Int
    parseCell c | c == '.'  = Nothing
                | otherwise = Just (ord c - ord '0')

-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency
         [(5, return Nothing),
          (4, do c <- choose (1,9); return (Just c))]

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-- property indicating if a Sudoku is valid
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku

-------------------------------------------------------------------------

type Block = [Maybe Int]

isOkayBlock :: Block -> Bool
isOkayBlock b = length withoutNothing == length (nub withoutNothing)
  where
    withoutNothing = filter isJust b

blocks :: Sudoku -> [Block]
blocks s = rows' ++ columns' ++ blocks'
  where
    rows'    = rows s
    columns' = transpose rows'
    blocks'  = filter(\x -> x/=[]) (rowWalker rows')

rowWalker :: [[Maybe Int]] -> [[Maybe Int]]
rowWalker r | length r == 3 = columnWalker $ take 3 r
            | otherwise     = columnWalker (take 3 r) ++ rowWalker (drop 3 r)

columnWalker :: [[Maybe Int]] -> [[Maybe Int]]
columnWalker ([]:[]:[]:[]) = [[]]
columnWalker (x:y:z:ws)    = ((take 3 x) ++ (take 3 y) ++ (take 3 z)) :
                                columnWalker ((drop 3 x) : (drop 3 y) : (drop 3 z) : ws)

isOkay :: Sudoku -> Bool
isOkay s = all isOkayBlock (blocks s)

prop_blocks :: Sudoku -> Bool
prop_blocks s = length blocks' == 27 && all (\b -> length b == 9) blocks'
  where
    blocks' = blocks s

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
