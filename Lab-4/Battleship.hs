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

data Cell = Cell Bool
   deriving ( Show )

type Coord = (Int, Char)

data Alignment = Hertical | Horizontal
   deriving (Eq, Show)

data Field = Field { rows :: [[Cell]] }
   deriving ( Show )

data Boat =
   AircraftCarrier | Battleship | Submarine | Destroyer | PatrolBoat
   { start :: Coord, alignment :: Alignment }
   deriving (Eq, Show)

data Fleet = Fleet { boats :: [Boat] }

-- Prints a field
printField :: Field -> IO ()
printField f = undefined

-- Checks for non-overlapping boats, space between boats
-- and size of the fleet
isValidFleet :: Fleet -> Bool
isValidFleet f = undefined

-- Gives a shiny, new and empty field ready for bombarment
brandNewField :: Field
brandNewField = undefined

-- Checks if all boats on a field are dying
allShipsSunken :: Field -> Bool
allShipsSunken f = undefined

-- Reads and parses a field form a file
readField :: FilePath -> IO Fleet
readField fp = undefined

-- Parses a string into a field
parseField :: String -> Fleet
parseField s = undefined

-- Gives the size of a given boat
sizeOfBoat :: Boat -> Int
sizeOfBoat b = undefined

-- Shoots at something on field with coordinates
shootAtSomething :: Field -> Coord -> Fleet -> Field
shootAtSomething field c fleet = undefined

