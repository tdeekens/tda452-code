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

type Coord = (Int, Char)

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
readField :: FilePath -> IO Fleet
readField fp = do
                  contents <- readFile fp
                  let field = parseField contents
                  if isField field
                    then return ( parseFleet field )
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
parseFleet f = undefined

-- Shoots at something on field with coordinates
shootAtSomething :: Field -> Coord -> Fleet -> Field
shootAtSomething field c fleet = undefined
