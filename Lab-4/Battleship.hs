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

{-
Haste
UI
parsing library for reading
play against a computer
computer is not stupid
testing
-}

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

-- A constant defining the size of a complete fleet
-- (sum of the sizes of all boats in a complete field)
sizeOfFleet = 19

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

{-
    Reading parsing etc 
-}



-- Reads and parses a field form a file
readField :: FilePath -> IO Fleet
--readField :: FilePath -> IO Field
readField fp = do
                  contents <- readFile fp
                  let field = parseField contents
                  if isField field
                    then return ( parseFleet field )
                    -- then return ( field )
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


-- Checks if the next cell in a row taken by a ship
checkNext :: Field -> Coord -> Bool
checkNext f (x,10) = False
checkNext f (x,y)  = case nextCell of
                      Nothing   -> False
                      Just True -> True
   where 
      r        = rows f
      nextCell = r!!x!!(y+1)
-- parses a field row, Int - row # of the Field, Coord - starting coord of the row
parseFieldRow :: Field -> Int -> [Boat]
parseFieldRow f rIdx | candidate == Nothing = []
                     | otherwise            
      = case (checkNext f (rIdx, fromJust(candidate))) of
               True -> [snd resH] ++ parseFieldRow (fst resH) rIdx
               False -> [snd resV] ++ parseFieldRow (fst resV) rIdx
   where 
      r         = (rows f)!!rIdx
      candidate = findIndex (==Just True) r
      resH      = readHorizontally f (rIdx, fromJust(candidate))
      resV      = readVertically f (rIdx, fromJust(candidate))

-- 
readVertically :: Field -> Coord -> (Field, Boat)
readVertically f c = undefined


readHorizontally :: Field -> Coord -> (Field, Boat)
readHorizontally f c = (newField, newBoat)
   where
      rIdx      = fst c
      cIdx      = snd c
      r         = (rows f)!!rIdx
      candidate = takeWhile (isJust) (drop cIdx r)
      newBoat   = craftBoat c Horizontal candidate
      newField  = updateField f [(rIdx,cIdx + (fromIntegral i)) | i <- [0..((sizeOfBoat newBoat) -1)] ] (Just False)