module Battleship where

import Test.QuickCheck
import Data.Maybe
import Data.List
import DataTypes

{-
   Lab Assignment 4
   Anna Averianova & Tobias Deekens, 2013
-}

-- Returns the size of a given boat
sizeOfBoat :: Boat -> Integer
sizeOfBoat (Boat AircraftCarrier _ _) = 5
sizeOfBoat (Boat Battleship _ _) = 4
sizeOfBoat (Boat Submarine _ _) = 3
sizeOfBoat (Boat Destroyer _ _) = 3
sizeOfBoat (Boat PatrolBoat _ _) = 2

-- Returns the size of a given boat model
sizeOfModel :: Model -> Integer
sizeOfModel AircraftCarrier = 5
sizeOfModel Battleship      = 4
sizeOfModel Submarine       = 3
sizeOfModel Destroyer       = 3
sizeOfModel PatrolBoat      = 2

-- Returns a new boat with provided starting coordinates, alignment and
-- a list of occupied coordinates
craftBoat :: Coord -> Alignment -> [Cell] -> Boat
craftBoat c a s | s' == 5 = Boat AircraftCarrier c a
                | s' == 4 = Boat Battleship c a
                | s' == 3 = Boat Destroyer c a
                | s' == 2 = Boat PatrolBoat c a
   where
     s' = length s

-- Returns an empty battlefield (untouched cells only)
emptyField :: Field
emptyField = Field (replicate 10 (replicate 10 Nothing))

-- Checks if all boats on a field have been hit
allShipsSunken :: Field -> Bool
allShipsSunken f = length (concat (map (filter (isJust)) r)) == sizeOfFleet
  where r = rows f

-- Returns an empty fleet
emptyFleet :: Fleet
emptyFleet = Fleet ([])

-- Returns a list of coordinates occupied by a boat on a field
boatCoord :: Boat -> [Coord]
boatCoord (Boat m (x,y) Horizontal) =
        [(x,y + fromIntegral(i)) | i <- [0..(sizeOfModel m -1)]]
boatCoord (Boat m (x,y) Vertical) =
        [(x + fromIntegral(i),y) | i <- [0..(sizeOfModel m -1)]]

-- Checks if coordinates of a boat do not go out the field borders
areBoatCoordOkay :: Boat -> Bool
areBoatCoordOkay (Boat m (x,y) a) = (x `elem` [0..9]) && (y `elem` [0..9])

-- Checks if the boat can be added to the fleet
-- Returns true when a new boat does not overlap with the boats in the fleet
-- and has valid field coordinates.
canBeInFleet :: Fleet -> Boat -> Bool
canBeInFleet f b = (fc `intersect` bc == []) && (areBoatCoordOkay b)
                    && (spaceLeftForModel f b)
  where fc = fleetCoord f
        bc = boatCoord b

-- Checks if a boat of a given type can be added to the fleet
-- Returns false when the fleet already has an upper limit of models
spaceLeftForModel :: Fleet -> Boat -> Bool
spaceLeftForModel f b  | (m == AircraftCarrier) || (m == Battleship)
                                = not (m `elem` fleetModels)
                       | (m == Destroyer) || (m == PatrolBoat)
                                = length (elemIndices m fleetModels) < 2
  where
      m           = model b
      fleetModels = [model b | b <- (boats f)]

-- Checks if a boat can be added to a fleet
-- and returns (updated fleet, True) if it is
-- (old fleet, False) otherwise
addToFleet :: Fleet -> Boat -> (Fleet, Bool)
addToFleet f b | canBeInFleet f b = (newFleet, True)
               | otherwise        = (f, False)
  where
    newFleet = Fleet ((boats f) ++ [b])

-- Returns a list of coordinates occupied by a fleet on a field
fleetCoord :: Fleet -> [Coord]
fleetCoord (Fleet [])     = []
fleetCoord (Fleet (x:xs)) = (boatCoord x) ++ (fleetCoord (Fleet xs))

-- Checks for non-overlapping boats, space between boats
-- and size of the fleet
isValidFleet :: Fleet -> Bool
isValidFleet f = undefined

-- Updates a field at a given set of coordinates with
-- the provided value
updateField :: Field -> [Coord] -> Maybe Bool -> Field
updateField f [] _     = f
updateField f (x:xs) b = let newField = updateCell f x b
                           in updateField newField xs b

-- Updates a Field at given coordinates with a provided value
updateCell :: Field -> Coord -> Maybe Bool -> Field
updateCell f x b = Field ( r !!= (rw, r!!rw !!= (cl, b)) )
   where
      r  = rows f
      rw = fst x
      cl = snd x

-- Given a list, and a tuple containing an index in the list and a new value
-- updates the given list with the new value at the given index
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) l (idx, t) | ( (length l) - 1 ) == idx = (fst $ splitAt idx l) ++ [t]
                 | otherwise                 = (fst chopped)
                                               ++ [t]
                                               ++ (tail $ snd chopped)
  where
    chopped = splitAt idx l

-- Shoots at a position on a field with provided coordinates
shootAtCoordinate :: Field -> Coord -> Fleet -> Field
shootAtCoordinate field c fleet
    | c `elem` (fleetCoord fleet) = updateField field [c] (Just True)
    | otherwise                   = updateField field [c] (Just False)
  where
    cell = (rows field)!!rIdx!!cIdx
    rIdx = fst c
    cIdx = snd c

example::Field
example =
   Field
     [ [Nothing, Just True, Just True, Just True, Just True, Just True, Nothing, Nothing, Nothing, Nothing]
     , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
     , [Nothing, Nothing, Nothing, Nothing, Just True, Just True, Just True, Just True, Nothing, Nothing]
     , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
     , [Nothing, Just True, Just True, Just True, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
     , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
     , [Nothing, Nothing, Nothing, Nothing, Nothing, Just True, Just True, Just True, Nothing, Nothing]
     , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
     , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
     , [Just True, Just True, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just True, Just True ]
     ]

exampleFleet :: Fleet
exampleFleet =
  Fleet {boats =
    [Boat {model = AircraftCarrier, start = (0,1), alignment = Horizontal},
     Boat {model = Battleship, start = (2,4), alignment = Horizontal},
     Boat {model = Destroyer, start = (4,1), alignment = Horizontal},
     Boat {model = Destroyer, start = (5,5), alignment = Vertical},
     Boat {model = PatrolBoat, start = (9,0), alignment = Horizontal},
     Boat {model = PatrolBoat, start = (9,8), alignment = Horizontal}]}
