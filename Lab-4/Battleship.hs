module Battleship where

import Test.QuickCheck
import Data.Maybe
import Data.List

{-
   Lab Assignment 4
   Anna Averianova & Tobias Deekens, 2013

   - Datatype field = Maybe Bool
      - Datatype board containing a [[Field]]
   - Datatype boat = AircraftCarrier | Battleship | Submarine |
                     Destroyer | PatrolBoat
      - Every boat knowing its horizontal/vertical alignment
      - and its position on the board
-}

-------------------------------------------------------------------------

type Cell = Maybe Bool

type Coord = (Int, Int)

data Alignment = Hertical | Horizontal
   deriving (Eq, Show)

data Field = Field { rows :: [[Cell]] }
   deriving ( Show )

data Model = AircraftCarrier | Battleship | Submarine | Destroyer | PatrolBoat
   deriving ( Show, Eq )

data Boat = Boat { model :: Model,
   start :: Coord,
   alignment :: Alignment }
   deriving (Eq, Show)

data Fleet = Fleet { boats :: [Boat] }

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

-- Checks for non-overlapping boats, space between boats
-- and size of the fleet
isValidFleet :: Fleet -> Bool
isValidFleet f = undefined

-- Gives the size of a given boat
sizeOfBoat :: Boat -> Integer
sizeOfBoat (Boat AircraftCarrier _ _) = 5
sizeOfBoat (Boat Battleship _ _) = 4
sizeOfBoat (Boat Submarine _ _) = 3
sizeOfBoat (Boat Destroyer _ _) = 3
sizeOfBoat (Boat PatrolBoat _ _) = 2

-- Gives a shiny, new and empty field ready for bombarment
brandNewField :: Field
brandNewField = Field (replicate 10 (replicate 10 Nothing))

-- Checks if all boats on a field are dying
allShipsSunken :: Field -> Bool
allShipsSunken f = undefined

-- Reads and parses a field form a file
--readField :: FilePath -> IO Fleet
readField :: FilePath -> IO Field
readField fp = do
                  contents <- readFile fp
                  let field = parseField contents
                  if isField field
                    -- then return ( parseFleet field )
                    then return ( field )
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

-- parses a field row, Int - row # of the Field, Coord - starting coord of the row
parseFieldRow :: Field -> Int -> [Boat]
parseFieldRow f rIdx | candidate == Nothing = []
                     | otherwise            = case (checkNext f (rIdx, fromJust(candidate))) of
                                                True -> [snd (readHorizontally f (rIdx, fromJust(candidate)))] ++ parseFieldRow f rIdx
                                                False -> [snd (readVertically f (rIdx, fromJust(candidate)))] ++ parseFieldRow f rIdx
   where 
      r         = (rows f)!!rIdx
      candidate = findIndex (==Just True) r

-- 
readVertically :: Field -> Coord -> (Field, Boat)
readVertically f c = undefined


readHorizontally :: Field -> Coord -> (Field, Boat)
readHorizontally f c = undefined{--takeWhile (not Nothing) candidate
   where
      rIdx = fst c
      cIdx = snd c
      r    = (rows f)!!rIdx
      candidate = drop cIdx r
      --}

craftBoat :: Coord -> Alignment -> [Cell] -> Boat
craftBoat c a s | s' == 5 = Boat AircraftCarrier c a
                | s'==4 = Boat Battleship c a
                | s'==3 = Boat Destroyer c a
                | s'==2 = Boat PatrolBoat c a
   where
     s' = length s

-- Checks if the next cell in a row taken by a ship
checkNext :: Field -> Coord -> Bool
checkNext f (x,10) = False
checkNext f (x,y)  = case nextCell of
                      Nothing   -> False
                      Just True -> True
   where 
      r        = rows f
      nextCell = r!!x!!(y+1)

-- Updates a filed at a given set of coordinates
updateField :: Field -> [Coord] -> Field
updateField f []     = f
updateField f (x:xs) = let newField = updateCell f x
                           in updateField newField xs

-- Updates a Field at given coordinates with a value False
updateCell :: Field -> Coord -> Field
updateCell f x = Field ( r !!= (rw, r!!rw !!= (cl, Just False)) )
   where
      r  = rows f
      rw = fst x
      cl = snd x

-- given a list, and a tuple containing an index in the list and a new value
-- updates the given list with the new value at the given index
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) l (idx, t) | ( (length l) - 1 ) == idx = (fst $ splitAt idx l) ++ [t]
                 | otherwise                 = (fst chopped)
                                               ++ [t]
                                               ++ (tail $ snd chopped)
  where
    chopped = splitAt idx l

-- Shoots at something on field with coordinates
shootAtSomething :: Field -> Coord -> Fleet -> Field
shootAtSomething field c fleet = undefined
