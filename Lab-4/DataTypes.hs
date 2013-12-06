module DataTypes where

import Test.QuickCheck
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

-- Generates random model
rModel :: Gen Model
rModel = frequency
      [(1, return AircraftCarrier),
       (1, return Battleship),
       (1, return Submarine),
       (1, return Destroyer),
       (2, return PatrolBoat)]

instance Arbitrary Model where
  arbitrary = rModel

-- Generates random alignment
rAlignment :: Gen Alignment
rAlignment = elements [Vertical, Horizontal]

instance Arbitrary Alignment where
  arbitrary = rAlignment

-- Generates random field coordinates
rCoord :: Gen (Int, Int)
rCoord = do
  x <- choose (0,9)
  y <- choose (0,9)
  return (x,y)

-- Generates a random boat
rBoat :: Gen Boat
rBoat = do
  m <- rModel
  c <- rCoord
  a <- rAlignment
  return (Boat m c a)

-- A constant defining the size of a complete fleet
-- (sum of the sizes of all boats in a complete field)
sizeOfFleet = 19 ::Int

-------------------------------------------------------------------------