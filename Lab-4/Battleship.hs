module Battleship where

import Data.Maybe
import Data.List
import Haste.Random
import DataTypes

{-
   Lab Assignment 4
   Anna Averianova & Tobias Deekens, 2013
-}

-- Returns the size of a given boat model
sizeOfModel :: Model -> Int
sizeOfModel AircraftCarrier = 5
sizeOfModel Battleship      = 4
sizeOfModel Submarine       = 3
sizeOfModel Destroyer       = 3
sizeOfModel PatrolBoat      = 2

-- Returns a new boat with provided starting coordinates, alignment and
-- a list of occupied coordinates
{-
  This is wrong now
-}
craftBoat :: Coord -> Alignment -> [Cell] -> Boat
craftBoat c a s | s' == 5 = Boat AircraftCarrier c a
                | s' == 4 = Boat Battleship c a
                | s' == 3 = Boat Destroyer c a
                | s' == 2 = Boat PatrolBoat c a
   where
     s' = length s

-- Flips a boat's alignment
flipAlignment :: Boat -> Boat
flipAlignment (Boat m s a) = Boat m s fa
  where
    fa = if a == Horizontal
            then Vertical
            else Horizontal

setStart :: Boat -> Coord -> Boat
setStart (Boat m s a) s' = Boat m s' a

-- Returns an empty battlefield (untouched cells only)
emptyField :: Field
emptyField = Field (replicate 10 (replicate 10 Nothing))

-- Returns an empty fleet
emptyFleet :: Fleet
emptyFleet = Fleet ([])

-- Checks if all boats on a field have been hit
allShipsSunken :: Field -> Bool
allShipsSunken f = length (concat (map (filter (==Just True)) r)) == sizeOfFleet
  where r = rows f

-- Returns a list of coordinates occupied by a boat on a field
boatCoord :: Boat -> [Coord]
boatCoord (Boat m (x,y) Horizontal) =
        [(x,y + fromIntegral(i)) | i <- [0..(sizeOfModel m -1)]]
boatCoord (Boat m (x,y) Vertical) =
        [(x + fromIntegral(i),y) | i <- [0..(sizeOfModel m -1)]]

-- Checks if coordinates of a boat do not go out the field borders
areBoatCoordOkay :: Boat -> Bool
areBoatCoordOkay b = and [(x `elem` [0..9]) && (y `elem` [0..9])
                            | (x,y) <- bc]
  where
    bc = boatCoord b

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
spaceLeftForModel f b  | (m == PatrolBoat)
                                = length (elemIndices m fleetModels) < 2
                       | otherwise
                                = not (m `elem` fleetModels)
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

-- Returns True only when a fleet consists of 6 non overlapping boats
-- of which one is AircraftCarrier, one is Battleship, one is Submarine,
-- one is Destroyer and two are PatrolBoat
isValidFleet :: Fleet -> Bool
isValidFleet f = (size == 6) && noIntersect
                  && length (elemIndices AircraftCarrier models) == 1
                  && length (elemIndices Battleship models) == 1
                  && length (elemIndices Submarine models) == 1
                  && length (elemIndices Destroyer models) == 1
                  && length (elemIndices PatrolBoat models) == 2
  where
    bs          = boats f
    size        = length bs
    boatsCoord  = concat [ boatCoord b| b <- bs]
    models      = [ model b| b <- bs]
    noIntersect = (nub boatsCoord) == boatsCoord

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
-- 0 == miss
-- 1 == hit
-- 2 == sink
shootAtCoordinate :: Field -> Coord -> Fleet -> (Field, Int)
shootAtCoordinate field c fleet
    | isNothing bh = (updateField field [c] (Just False), 0)
    | otherwise    = case isBoatSunk field (bc \\ [c]) of
                        False -> (updateField field [c] (Just True), 1)
                        True  -> (updateField field [c] (Just True), 2)
  where
    cell = (rows field)!!rIdx!!cIdx
    rIdx = fst c
    cIdx = snd c
    bh   = whichBoatHit c fleet
    bc   = boatCoord (fromJust bh)

-- Checks which boat in the fleet has a given coordinate
-- returns Nothing if no boat with such coordinate are in the fleet
whichBoatHit :: Coord -> Fleet -> Maybe Boat
whichBoatHit c f | b == Nothing = Nothing
                 | otherwise    = Just ((boats f)!!(fromJust b))
  where
    fcl = map boatCoord $ boats f
    b   = findIndex (isJust) $ map (elemIndex c) fcl

-- Checks if a hit sinks the boat
isBoatSunk :: Field -> [Coord] -> Bool
isBoatSunk f c = all (\x -> isJust (r!!(fst x)!!(snd x))) c
  where
    r = rows f

-------------------------------------------------------------------------
-- All possible shots
fullShots :: [(Coord)]
fullShots = [(i,j) | i <- [0..9], j <- [0..9]]

-- Shuffles the shots to be in random order
shuffleShots :: Seed -> [(Coord)] -> [(Coord)]
shuffleShots _ [] = []
shuffleShots g s  = item : shuffleShots g' rest
  where
    (pos, g') = randomR (0, length s - 1) g
    item      = s!!pos
    rest      = delete item s

-- Takes a current number of shots, a field, a fleet and a coord where
-- the ship has been hit at
sinkShip :: [Direction] -> Field -> Fleet -> Coord -> [Coord] -> (Field, [Coord])
sinkShip dirs fd ft c shs | (trd' try) = sinkShip ds (snd' try) ft c shs'
                          | otherwise  = ( (snd' try), shs')
  where
    d    = head dirs
    ds   = tail dirs
    try  = walkSide d fd ft c []
    shs' = shs\\(fst' try)

-- Temp, updates the field at the initial coordinate
-- (in the future, assumed that the filed is already updated with this hit)
sinkShip' :: [Direction] -> Field -> Fleet -> Coord -> [Coord] -> (Field, [Coord])
sinkShip' dirs fd ft c shs | (trd' try) = sinkShip' ds (snd' try) ft c shs'
                           | otherwise  = ( (snd' try), shs')
  where
    d    = head dirs
    ds   = tail dirs
    try  = walkSide d fd' ft c []
    fd'  = updateField fd [c] (Just True)
    shs' = shs\\(fst' try)

-- Shoots the filed in a given direction.
-- Returns a list of shots (empty when a shot in given direction is impossibe,
-- updated field and
-- True if the ship not sunk and another direction walk needed
-- False if the ship has been sunk
walkSide :: Direction -> Field -> Fleet -> Coord -> [Coord] -> ([Coord], Field, Bool)
walkSide d fd ft c sh | t == Nothing   = ([], fd, True)
                      | (snd res) == 1 = walkSide d (fst res) ft t' (t':sh)
                      | (snd res) == 2 = (t':sh, (fst res), False)
                      | (snd res) == 0 = (t':sh, (fst res), True)
   where
     t   = nextTarget d c
     t'  = fromJust t
     res = shootAtCoordinate fd (fromJust t) ft

-- Returns a next coordinate in a given direction
-- Returns Nothing if next cell does not exist
nextTarget :: Direction -> Coord -> Maybe Coord
nextTarget North (0,_) = Nothing
nextTarget North (x,y) = Just (x-1,y)
nextTarget South (9,_) = Nothing
nextTarget South (x,y) = Just (x+1,y)
nextTarget East  (_,9) = Nothing
nextTarget East  (x,y) = Just (x,y+1)
nextTarget West  (_,0) = Nothing
nextTarget West  (x,y) = Just (x,y-1)

fst':: (a,b,c) -> a
fst' (a,_,_) = a

snd':: (a,b,c) -> b
snd' (_,b,_) = b

trd':: (a,b,c) -> c
trd' (_,_,c) = c

-------------------------------------------------------------------------

-- Example Field
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

-- Example Fleet
exampleFleet :: Fleet
exampleFleet =
  Fleet {boats =
    [Boat {model = AircraftCarrier, start = (0,1), alignment = Horizontal},
     Boat {model = Battleship, start = (2,4), alignment = Horizontal},
     Boat {model = Submarine, start = (4,1), alignment = Horizontal},
     Boat {model = Destroyer, start = (5,5), alignment = Vertical},
     Boat {model = PatrolBoat, start = (9,0), alignment = Horizontal},
     Boat {model = PatrolBoat, start = (9,8), alignment = Horizontal}]}
-------------------------------------------------------------------------

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
