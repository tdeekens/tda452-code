module Parser where

import DataTypes
import Battleship
import Data.Maybe
import Data.List

{-
-- Prints a field
printField :: Field -> IO ()
printField f = putStr (unlines (map printRow rows'))
  where
    rows' = rows f

-- Helper function printing a list of field cells on the screen
printRow :: [Cell] -> String
printRow = foldr ((++) . printCell) ""

-- Helper function printing a field cell on the screen
printCell :: Cell -> String
printCell Nothing   = "_"
printCell (Just True)  = "x"
printCell (Just False)  = "."
-}


{-
    Reading parsing etc 
-}

-- Reads and parses a field form a file
readField :: FilePath -> IO Fleet
--readField :: FilePath -> IO Field
readField fp = do
                  contents <- readFile fp
                  let field = parseField contents
                  if isField field
                    then return ( parseFleet field )
                    -- then return ( field )
                    else error "File containing invalid field."

-- Checks if a f is a valid representation of a Field
isField :: Field -> Bool
isField f = rowCount == 10 && all isValidRow rows'
  where
    rows'     = rows f
    rowCount  = length rows'

isValidRow :: [Cell] -> Bool
isValidRow r = all (\ x -> case x of
                      Nothing    -> True
                      Just True  -> True
                      _          -> False) r

-- Parses a string into a field
parseField :: String -> Field
parseField s = Field (map parseRow (lines s))

-- parses a f row from a string
parseRow :: String -> [Cell]
parseRow = map parseCell

-- parses a f cell from a char
parseCell :: Char -> Cell
parseCell c | c == '_'  = Nothing
            | otherwise = Just True

-- Parses a Field into a Fleet
parseFleet :: Field -> Fleet
parseFleet f = Fleet (concat [parseFieldRow f idx | idx <- [0..9]])
   where 
      r = rows f


-- Checks if the next cell in a row taken by a ship
checkNext :: Field -> Coord -> Bool
checkNext f (x,10) = False
checkNext f (x,y)  = case nextCell of
                      Nothing   -> False
                      Just True -> True
   where 
      r        = rows f
      nextCell = r!!x!!(y+1)
-- parses a field row, Int - row # of the Field, Coord - starting coord of the row
parseFieldRow :: Field -> Int -> [Boat]
parseFieldRow f rIdx | candidate == Nothing = []
                     | otherwise            
      = case (checkNext f (rIdx, fromJust(candidate))) of
               True -> [snd resH] ++ parseFieldRow (fst resH) rIdx
               False -> [snd resV] ++ parseFieldRow (fst resV) rIdx
   where 
      r         = (rows f)!!rIdx
      candidate = findIndex (==Just True) r
      resH      = readHorizontally f (rIdx, fromJust(candidate))
      resV      = readVertically f (rIdx, fromJust(candidate))

-- 
readVertically :: Field -> Coord -> (Field, Boat)
readVertically f c = undefined


readHorizontally :: Field -> Coord -> (Field, Boat)
readHorizontally f c = (newField, newBoat)
   where
      rIdx      = fst c
      cIdx      = snd c
      r         = (rows f)!!rIdx
      candidate = takeWhile (isJust) (drop cIdx r)
      newBoat   = craftBoat c Horizontal candidate
      newField  = updateField f [(rIdx,cIdx + (fromIntegral i)) | i <- [0..((sizeOfModel (model newBoat)) -1)] ] (Just False)