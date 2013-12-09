module Properties where

import Battleship
import DataTypes
import Test.QuickCheck
import Data.List

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