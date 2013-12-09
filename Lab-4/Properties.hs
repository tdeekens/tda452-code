module Properties where

import Battleship
import DataTypes
import Test.QuickCheck
import Data.List


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

instance Arbitrary Boat where
	arbitrary = rBoat

-- Generates a fleet

rFleet :: Gen Fleet
rFleet = do
  b1 <- rBoat
  b2 <- rBoat
  b3 <- rBoat
  b4 <- rBoat
  b5 <- rBoat
  b6 <- rBoat
  return (Fleet [b1,b2,b3,b4,b5])

instance Arbitrary Fleet where
	arbitrary = rFleet
	
prop_boatCoord :: Boat -> Bool
prop_boatCoord b = (length coords == (sizeOfModel $ model b)) &&
					case a of
						Vertical -> (nub cCs) == [snd(start b)] && isOrdered rCs
						Horizontal -> (nub rCs) == [fst(start b)] && isOrdered cCs
	where 
		coords = boatCoord b
		cs     = unzip coords
		rCs    = fst cs
		cCs    = snd cs
		a      = alignment b
	    
-- Checks if the items in the list are ordered
isOrdered :: Ord a => [a] -> Bool
isOrdered xs = and $ zipWith (<=) xs (tail xs)

-- Checks if Field read from a file is a valid representation of a field
prop_isField = undefined