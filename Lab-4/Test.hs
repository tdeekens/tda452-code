module Test where

import DataTypes
import Data.Maybe
import Data.Char
import Data.List

boatFromJSState :: String -> Boat
boatFromJSState s = Boat m cs a
   where
     state    = s
     splitted = splitUntil (== '|') s
     a        = alignmentByState (splitted!!0)
     cs       = coordByState (splitted!!1)
     m        = modelByState (splitted!!2)

-- Generates alignment from a String
alignmentByState :: String -> Alignment
alignmentByState "horizontal" = Horizontal
alignmentByState otherwise    = Vertical

-- Generates a model from a String
modelByState :: String -> Model
modelByState "aircraftcarrier" = AircraftCarrier
modelByState "battleship"      = Battleship
modelByState "submarine"       = Submarine
modelByState "destroyer"       = Destroyer
modelByState otherwise      = PatrolBoat

-- Generates coords by the state's String
coordByState :: String -> Coord
coordByState cs = (read x::Int, read y::Int)
  where
    cs' = splitUntil (== '-') cs
    x   = cs'!!0
    y   = cs'!!1

splitUntil :: (Char -> Bool) -> String -> [String]
splitUntil p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitUntil p s''
                            where (w, s'') = break p s'