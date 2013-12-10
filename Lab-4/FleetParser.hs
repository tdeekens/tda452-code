module FleetParser where

import DataTypes
import Data.Maybe
import Data.List
import Data.Char

type Parser a = String -> Maybe (a, String)

pModel :: Parser Model
pModel (c:s) | isLetter c = chars [c] s
pModel _                  = Nothing

chars :: String -> String -> Maybe (Model, String)
chars ls (c:s) | isLetter c       = chars (ls ++ [c]) s
chars ls s     | fst (isModel ls) = Just (snd (isModel ls), s)
               | otherwise        = Nothing

isModel :: String -> (Bool, Model)
isModel "AircraftCarrier" = (True, AircraftCarrier)
isModel "Battleship"      = (True, Battleship)
isModel "Submarine"       = (True, Submarine)
isModel "Destroyer"       = (True, Destroyer)
isModel "PatrolBoat"      = (True, PatrolBoat)
isModel _                 = (False, undefined)

number :: Parser Int
number (c:s) | isDigit c = Just (digits 0 (c:s))
number _                 = Nothing

digits :: Int -> String -> (Int,String)
digits n (c:s) | isDigit c = digits (10*n + digitToInt c) s
digits n s                 = (n,s)

pCoord :: Parser Coord
pCoord (' ':cs) = pCoord cs
pCoord ('(':s)  =
	case number s of
		Just (a, ',':s') -> case number s' of
			                   Just (b,')':s'') -> Just ((a,b), s'')
			                   _                -> Nothing
		_                -> Nothing
pCoord _       = Nothing

pAlignment :: Parser Alignment
pAlignment (' ':cs) = pAlignment cs
pAlignment ('V':cs) = Just (Vertical, cs)
pAlignment ('H':cs) = Just (Horizontal, cs)
pAlignment _        = Nothing

pBoat :: Parser Boat
pBoat s = case pModel s of
			Just (m, s') -> case pCoord s' of
				              Just (c, s'') -> case pAlignment s'' of
				              	                Just (a, s''') -> Just (Boat m c a, s''')
				              	                _              -> Nothing
				              _             -> Nothing
			Nothing      -> Nothing