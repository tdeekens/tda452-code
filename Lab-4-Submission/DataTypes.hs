module DataTypes where

import Data.Maybe
import Data.List

-------------------------------------------------------------------------

-- Type synonym for representation of a field cell
-- Nothing    == unctouched cell,
-- Just True  == hit cell
-- Just False == missed cell
type Cell = Maybe Bool

-- Type synonym for cell coordinates on a field
type Coord = (Int, Int)

-- Alignment of a boat on a filed
data Alignment = Vertical | Horizontal
   deriving (Eq, Show)

-- 10x10 battle field
data Field = Field { rows :: [[Cell]] }
   deriving ( Show )

-- Models of boats that could be placed on a field
data Model = AircraftCarrier | Battleship | Submarine | Destroyer | PatrolBoat
   deriving ( Show, Eq )

-- A boat placed on a field is described by its model, starting coordinates
-- and alignment
data Boat = Boat { model :: Model,
   start :: Coord,
   alignment :: Alignment }
   deriving (Eq, Show)

-- A list of boats that are used in one game
data Fleet = Fleet { boats :: [Boat] }
   deriving ( Show )

-- Directions for shooting
data Direction =  North | South | West | East
   deriving (Enum, Eq, Show)

directions = [North, South, West, East] :: [Direction]

-- A constant defining the size of a complete fleet
-- (sum of the sizes of all boats in a complete field)
sizeOfFleet = 19 ::Int

-------------------------------------------------------------------------