module Main where
import Haste
import Control.Monad(when)
import DataTypes
import Battleship
import Data.Maybe
import Data.Char

newtype JQuery = JQuery JSAny

-- Gives jQuery from JS side a things
foreign import ccall js_jquery :: JSString -> IO (JQuery)

-- Click callback from JS
foreign import ccall js_click :: JQuery -> JSFun (JSString -> IO ()) -> IO ()
foreign import ccall js_unbind :: JQuery -> IO ()

-- All the functions which can be ran from Haskell
foreign import ccall selectBoat :: JSString -> IO ()
foreign import ccall flipBoat   :: JSString -> IO ()
foreign import ccall lockBoat   :: JSString -> IO ()
foreign import ccall startGame  :: JSString -> IO ()
foreign import ccall resetGame  :: JSString -> IO ()
foreign import ccall addBoat    :: JSString -> IO ()
foreign import ccall getState   :: JSString
foreign import ccall markHit    :: JSString -> IO ()

-- Define jQuery as j and a function binding an action onto it on the JS side
j :: String -> (JQuery -> IO ()) -> IO ()
j s action = js_jquery (toJSString s) >>= action

-- Register an onclick callback evaluating the .
click :: (JSString -> IO ()) -> JQuery -> IO ()
click f jq = js_click jq (mkCallback f)

-- Unregister an onclick callback evaluating the .
unbind :: JQuery -> IO ()
unbind jq = js_unbind jq

-- Queries JS's state and creates a boat from the currently selected one
boatFromJSState :: Boat
boatFromJSState = Boat m cs a
   where
     state    = fromJust (fromJSString $ getState)
     splitted = split' state '|'
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
modelByState "patrolboat"      = PatrolBoat

-- Generates coords by the state's String
coordByState :: String -> Coord
coordByState cs = (read x::Int, read y::Int)
  where
    cs' = split' cs '-'
    x   = cs'!!0
    y   = cs'!!1

-- Helper function splitting up a string by a delimiter
split' :: String -> Char -> [String]
split' [] delim       = []
split' (c:cs) delim | c == delim = "" : rest
                    | otherwise = (c : head rest) : tail rest
  where
      rest = split' cs delim

-- Proxy adding a boat to the field in JS only if possible
addBoatProxy :: JSString -> IO ()
addBoatProxy idx = do
                  let yn = canBeInFleet emptyFleet b'
                  if yn
                    then
                      addBoat idx
                    else alert "Invalid position for boat!"
  where
    b' = boatFromJSState

-- Proxy flipping a boat only if possible
flipBoatProxy :: JSString -> IO ()
flipBoatProxy idx = do
                  let yn = canBeInFleet emptyFleet b'
                  if yn
                    then
                      flipBoat idx
                    else alert "Boat can not be flipped at position!"
  where
    b' = flipAlignment boatFromJSState

-- Proxy only locking/adding a boat to the fleet if possible
lockBoatProxy :: JSString -> IO ()
lockBoatProxy idx = do
                  let (f, yn) = addToFleet emptyFleet b'
                  if yn
                    then
                      flipBoat idx
                    else alert "Boat could not be added to fleet!"
  where
    b' = boatFromJSState

-- Proxy only starting the game if fleet is valid
startGameProxy :: JSString -> IO ()
startGameProxy idx = do
                  let yn = isValidFleet emptyFleet
                  if yn
                    then do
                            j "tbody td:not(.shead)"unbind
                            j "tbody td:not(.shead)" $
                              click (\idx -> shootProxy idx)
                            startGame idx
                    else alert "Fleet not yet valid, game can't be started!"
  where
    b' = boatFromJSState

-- Proxy shooting at field and piping result to JS dependent on outcome
shootProxy :: JSString -> IO ()
shootProxy idx = do
                  let (f, o) = shootAtCoordinate undefined cs emptyFleet
                  if o > 0
                    then
                      markHit idx
                    else alert "Haha, n00b you missed!"
  where
    idx' = fromJSString idx
    cs   = case isNothing idx' of
             False -> coordByState (fromJust idx')
             True  -> (-1, -1)

-- Lets get down to binding to JS functions
-- When clauses are for hooking into game logic
main = do
  j "tbody td:not(.shead)" $
    click (\idx -> addBoatProxy idx)
  j "button#flip" $
      click (\idx -> flipBoatProxy idx)
  j "button#lock" $
    click (\idx -> lockBoatProxy idx)
  j "button#start" $
    click (\idx -> startGameProxy idx)
  j ".boat" $
    click (\idx -> selectBoat idx)
  j "button#reset" $
    click (\idx -> resetGame idx)
