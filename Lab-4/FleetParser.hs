module FleetParser where

import DataTypes
import Data.Maybe
import Data.List
import Data.Char

type Parser a = String -> Maybe (a, String)

-- Parses a model from a string
pModel :: Parser Model
pModel (c:s) | isLetter c = chars [c] s
pModel _                  = Nothing

-- Extracts a sting of alphabetical letters,
-- checks if the string corresponds to one of the model names,
-- returns a Model if it does, otherwise returns Nothing
chars :: String -> String -> Maybe (Model, String)
chars ls (c:s) | isLetter c       = chars (ls ++ [c]) s
chars ls s     | fst (isModel ls) = Just (snd (isModel ls), s)
               | otherwise        = Nothing

-- Checks if a String corresponds to one of the model names
isModel :: String -> (Bool, Model)
isModel "AircraftCarrier" = (True, AircraftCarrier)
isModel "Battleship"      = (True, Battleship)
isModel "Submarine"       = (True, Submarine)
isModel "Destroyer"       = (True, Destroyer)
isModel "PatrolBoat"      = (True, PatrolBoat)
isModel _                 = (False, undefined)

-- Parses an Int from a string
number :: Parser Int
number (c:s) | isDigit c = Just (digits 0 (c:s))
number _                 = Nothing

-- Returns an Int represented by a subset of a String
digits :: Int -> String -> (Int,String)
digits n (c:s) | isDigit c = digits (10*n + digitToInt c) s
digits n s                 = (n,s)

-- Parses a Coord from a String (in the form of (Int, Int) )
pCoord :: Parser Coord
pCoord (' ':cs) = pCoord cs
pCoord ('(':s)  =
	case number s of
		Just (a, ',':s') -> case number s' of
			                   Just (b,')':s'') -> Just ((a,b), s'')
			                   _                -> Nothing
		_                -> Nothing
pCoord _       = Nothing

-- Parses an Alignment from a string
pAlignment :: Parser Alignment
pAlignment (' ':cs) = pAlignment cs
pAlignment ('V':cs) = Just (Vertical, cs)
pAlignment ('H':cs) = Just (Horizontal, cs)
pAlignment _        = Nothing

-- Parses a Boat from a string
pBoat :: Parser Boat
pBoat s = case pModel s of
			Just (m, s') -> case pCoord s' of
				              Just (c, s'') -> case pAlignment s'' of
				              	                Just (a, s''') -> Just (Boat m c a, s''')
				              	                _              -> Nothing
				              _             -> Nothing
			Nothing      -> Nothing

-- Parses a Fleet (list of Boats) from a string
pFleet :: Parser Fleet
pFleet s = case pBoat s of
             Just (b, c:s') | c == '\n' ->
	                   case pFleet s' of 
                        Just (f',s'') -> Just (Fleet (b:(boats f')), s'')
                        Nothing       -> Just (Fleet [b], s')
                            | otherwise -> Nothing
             Nothing -> Nothing

-- Reads a file and tries to parse a fleet
readAndSolve :: FilePath -> IO ()
readAndSolve fp = do
                   contents <- readFile fp
                   let f = pFleet contents
                   if (isJust f)
                   	then
                        if (isValidFleet (fst(fromJust f)))
                        then print f
                        else error "not valid fleet"
                   	else print f