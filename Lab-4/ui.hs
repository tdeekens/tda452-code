module Main where
import Haste
import Control.Monad (when)
import DataTypes
import Battleship
import Data.Maybe
import Data.Char

newtype JQuery = JQuery JSAny

-- Gives jQuery from JS side a things
foreign import ccall js_jquery :: JSString -> IO (JQuery)

-- Click callback from JS
foreign import ccall js_click :: JQuery -> JSFun (JSString -> IO ()) -> IO ()

-- All the functions which can be ran from Haskell
foreign import ccall selectBoat :: JSString -> IO ()
foreign import ccall flipBoat :: JSString -> IO ()
foreign import ccall lockBoat :: JSString -> IO ()
foreign import ccall startGame :: JSString -> IO ()
foreign import ccall resetGame :: JSString -> IO ()
foreign import ccall addBoat :: JSString -> IO ()
foreign import ccall markHorizontal :: Int -> IO ()
foreign import ccall getState :: JSString

-- Define jQuery as j and a function binding an action onto it on the JS side
j :: String -> (JQuery -> IO ()) -> IO ()
j s action = js_jquery (toJSString s) >>= action

-- Register an onclick callback evaluating the .
click :: (JSString -> IO ()) -> JQuery -> IO ()
click f jq = js_click jq (mkCallback f)

boatFromJSState :: Boat
boatFromJSState = Boat m cs a
   where
     state    = fromJust (fromJSString $ getState)
     splitted = split' state '|'
     a        = alignmentByState (splitted!!0)
     cs       = coordByState (splitted!!1)
     m        = modelByState (splitted!!2)

alignmentByState :: String -> Alignment
alignmentByState "horizontal" = Horizontal
alignmentByState otherwise    = Vertical

modelByState :: String -> Model
modelByState "aircraftcarrier" = AircraftCarrier
modelByState "battleship"      = Battleship
modelByState "submarine"       = Submarine
modelByState "destroyer"       = Destroyer
modelByState "patrolboat"     = PatrolBoat

coordByState :: String -> Coord
coordByState cs = (read x::Int, read y::Int)
  where
    cs' = split' cs '-'
    x   = cs'!!0
    y   = cs'!!1

split' :: String -> Char -> [String]
split' [] delim = []
split' (c:cs) delim
  | c == delim = "" : rest
  | otherwise = (c : head rest) : tail rest
  where
      rest = split' cs delim

addBoatProxy :: JSString -> IO ()
addBoatProxy b = do
                  if canBeInFleet emptyFleet b'
                    then selectBoat b
                    else alert "Boat can not be added!"
  where
    b' = boatFromJSState

-- Lets get down to binding to JS functions
-- When clauses are for hooking into game logic
main = do
  j ".boat" $
    click (\idx
              -> when (True) (selectBoat idx))
  j "button#flip" $
      click (\idx
              -> when (True) (flipBoat idx))
  j "button#lock" $
    click (\idx
              -> when (True) (lockBoat idx))
  j "button#start" $
    click (\idx
              -> when (True) (startGame idx))
  j "button#reset" $
    click (\idx
              -> when (True) (resetGame idx))
  j ".boat" $
    click (\idx
              -> when (True) (selectBoat idx))
  j "tbody td:not(.shead)" $
    click (\idx
              -> when (True) (addBoatProxy idx))