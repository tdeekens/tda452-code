module Properties where

import Battleship
import DataTypes
import Generators
import Test.QuickCheck
import Data.List
import System.Random

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

prop_areBoatCoordOkay :: Boat -> Bool
prop_areBoatCoordOkay b = 
	case a of 
	   Vertical   -> and [ (x + i) <= 9 | i <- [0..sizeOfModel m -1]] == areBoatCoordOkay b
	   Horizontal -> and [ (y + i) <= 9 | i <- [0..sizeOfModel m -1]] == areBoatCoordOkay b
   where
     (x,y) = start b
     m     = model b
     a     = alignment b

prop_shuffleShots :: StdGen -> Coord -> [Coord] -> Bool
prop_shuffleShots g c cs =
   (c `elem` cs) == (c `elem` (shuffleShots g cs))

prop_sizeShuffleShots :: StdGen -> [Coord] -> Bool
prop_sizeShuffleShots g cs =
   length cs == length (shuffleShots g cs)

-- Does not take into account validity of a fleet due to stupidity of its generator
prop_fleetCoord :: Fleet -> Bool
prop_fleetCoord f = and (map (\x -> and [(c `elem` fc) | c <- (boatCoord x)]) b)
   where
   	b  = boats f
   	fc = fleetCoord f

prop_updateCell :: Field -> Coord -> Maybe Bool -> Bool
prop_updateCell f (x,y) v = (r'!!x'!!y') == v
   where
     x' = abs (x `mod` 10)
     y' = abs (y `mod` 10)
     r' = rows (updateCell f (x',y') v)

prop_updateField :: Field -> [Coord] -> Maybe Bool -> Bool
prop_updateField f cs v = and $ map (\(x, y) -> (r'!!x!!y) == v) cs'
  where
      cs' = map (\(x, y) -> (abs (x `mod` 10), abs (y `mod` 10))) cs
      r'  = rows (updateField f cs' v)

fewerChecks prop = quickCheckWith stdArgs{ maxSuccess = 3 } prop