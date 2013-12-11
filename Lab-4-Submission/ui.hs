module Main where

import Haste
import Control.Monad(when)
import DataTypes
import Data.Maybe
import Data.Char
import BattleshipHaste
import Data.List
import Haste.Random
import Control.Monad.ST
import Data.IORef

newtype JQuery = JQuery JSAny
data State = State Fleet

-- Gives jQuery from JS side a things
foreign import ccall js_jquery :: JSString -> IO (JQuery)

-- Click callback from JS
foreign import ccall js_click  :: JQuery -> JSFun (JSString -> JSString -> IO ()) -> IO ()
foreign import ccall js_unbind :: JQuery -> IO ()

-- All the functions which can be ran from Haskell
foreign import ccall selectBoat :: JSString -> IO ()
foreign import ccall flipBoat   :: JSString -> IO ()
foreign import ccall lockBoat   :: JSString -> IO ()
foreign import ccall startGame  :: JSString -> IO ()
foreign import ccall resetGame  :: JSString -> IO ()
foreign import ccall addBoat    :: JSString -> IO ()
foreign import ccall markHit    :: JSString -> IO ()
foreign import ccall markMiss   :: JSString -> IO ()
foreign import ccall message    :: JSString -> IO ()
foreign import ccall debug      :: JSString -> IO ()
foreign import ccall finish     :: JSString -> IO ()

-- Define jQuery as j and a function binding an action onto it on the JS side
j :: String -> (JQuery -> IO ()) -> IO ()
j s action = js_jquery (toJSString s) >>= action

-- Register an onclick callback evaluating the .
click :: (JSString -> JSString -> IO ()) -> JQuery -> IO ()
click f jq = js_click jq (mkCallback f)

-- Unregister an onclick callback evaluating the .
unbind :: JQuery -> IO ()
unbind jq = js_unbind jq

-- Queries JS's state and creates a boat from the currently selected one
boatFromJSState :: JSString -> Boat
boatFromJSState s = Boat m cs a
   where
    state    = fromJust (fromJSString $ s)
    splitted = splitUntil (== '|') state
    a        = alignmentByState (splitted!!2)
    cs       = coordByState (splitted!!1)
    m        = modelByState (splitted!!0)

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
modelByState otherwise         = PatrolBoat

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

-------------------------------------------------------------------------------------------

-- Proxy adding a boat to the field in JS only if possible
addBoatProxy :: JSString -> JSString -> IORef State -> IO ()
addBoatProxy idx s r = do
                      (State f) <- readIORef r
                      debug $ toJSString ("Trying to add " ++ show b'' ++ ".")
                      if (canBeInFleet f b'')
                        then
                          addBoat idx
                        else message $ toJSString "Invalid position for boat!"
  where
    idx' = fromJust (fromJSString $ idx)
    b'   = boatFromJSState s
    b''  = setStart b' (coordByState (drop 1 idx'))

-- Proxy flipping a boat only if possible
flipBoatProxy :: JSString -> JSString -> IORef State -> IO ()
flipBoatProxy idx s r = do
                        (State f) <- readIORef r
                        debug $ toJSString ("Trying to flip " ++ show b' ++ ".")
                        if (canBeInFleet f b'')
                          then
                            flipBoat idx
                          else message $ toJSString "Boat can not be flipped at position!"
  where
    b'   = boatFromJSState s
    b''  = flipAlignment b'

-- Proxy only locking/adding a boat to the fleet if possible
lockBoatProxy :: JSString -> JSString -> IORef State -> IO ()
lockBoatProxy idx s r = do
                        (State f) <- readIORef r
                        let (f', yn) = addToFleet f b'
                        if yn
                          then do
                            writeIORef r (State f')
                            debug $ toJSString (show b' ++ " locked!")
                            debug $ toJSString ("New " ++ show f' ++ ".")
                            message $ toJSString ("Boat locked, go on!")
                            lockBoat idx
                          else message $ toJSString "Boat could not be added to fleet!"
  where
    b' = boatFromJSState s

-- Proxy only starting the game if fleet is valid
startGameProxy :: JSString -> JSString -> IORef State -> IO ()
startGameProxy idx _ r = do
                          (State f) <- readIORef r
                          if isValidFleet f
                            then do
                                  runGame f
                                  --startGame idx
                            else message $ toJSString "Fleet not yet valid, game can't be started!"

-- Draws results of firing a round
drawShotResults :: [Coord] -> Fleet -> IO ()
drawShotResults []     f               = do
                                            debug $ toJSString "Sunk one. Haha!"
                                            return ()
drawShotResults (c:cs) f | c `elem` fc = do
                                            markHit c'
                                            drawShotResults cs f
                         | otherwise   = do
                                            markMiss c'
                                            drawShotResults cs f
  where
    fc     = fleetCoord f
    (x, y) = c
    idx    = (show x) ++ "-" ++ (show y)
    c'     = toJSString idx

-- Lets get down to binding to JS functions
bindUI :: IORef State -> IO ()
bindUI r = do
  j "tbody td:not(.shead)" $
    click (\idx s -> addBoatProxy idx s r)
  j "button#flip" $
    click (\idx s -> flipBoatProxy idx s r)
  j "button#lock" $
    click (\idx s -> lockBoatProxy idx s r)
  j "button#start" $
    click (\idx s -> startGameProxy idx s r)
  j ".boat" $
    click (\idx s -> selectBoat idx)
  j "button#reset" $
    click (\idx s -> resetGame idx)

-------------------------------------------------------------------------------------------

-- Starts the game by creating shots and kicking off the game loop
runGame :: Fleet -> IO ()
runGame f = do
  let g = mkSeed 42 -- doesnt return IO Seed
  let shots = shuffleShots g fullShots
  gameLoop 0 emptyField f shots

gameLoop :: Int -> Field -> Fleet -> [(Coord)] -> IO ()
gameLoop num field fleet []    = do message $ toJSString "Darn. Couldnt kill you!"
gameLoop num field fleet shots =
  do
    case (allShipsSunken field) of
      True -> do
               finish (toJSString (show 42)) -- ifs its num, haste crashes
      False -> do
                let c       = head shots
                    (x, y)  = c
                    c'      = (show x) ++ "-" ++ (show y)
                    res     = shootAtCoordinate field c fleet
                case snd res of
                  0 -> do
                         markMiss $ toJSString c'
                         gameLoop (num + 1) (fst res) fleet (tail shots)
                  1 -> do
                         let sinkRes = sinkShip directions (fst res) fleet c (tail shots)
                         let us = (length shots) - (length (snd sinkRes))
                         -- haste crashes if this remains in, tested without haste
                         --drawShotResults (shots \\ (snd sinkRes)) fleet
                         gameLoop (num + us) (fst sinkRes) fleet (snd sinkRes)
                  2 -> do
                         debug $ toJSString "Sunk one. Haha!"
                         gameLoop (num + 1) (fst res) fleet (tail shots)

-------------------------------------------------------------------------------------------

main :: IO ()
main = do
          s <- newIORef (State emptyFleet)
          bindUI s