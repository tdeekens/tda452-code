module Wrapper where

import Data.Char
import Data.List
import System.Random
import DataTypes

data Interface = Interface
  { iEmptyField :: Field
  , iEmptyFleet :: Fleet
  , iExampleFleet :: Fleet
  , iShootAtCoordinate :: Field -> Coord -> Fleet -> (Field, Int)
  , iAllShipsSunken :: Field -> Bool
  , iPrintField :: Field -> IO ()
  , iShuffleShots :: StdGen -> [(Coord)] -> [(Coord)]
  , iFullShots :: [(Coord)]
  }

{-
-- All possible shots
shots :: [(Coord)]
shots = [(i,j) | i <- [0..9], j <- [0..9]]

-- Shuffles the shots to be in random order
shuffleShots :: StdGen -> [(Coord)] -> [(Coord)]
shuffleShots _ [] = []
shuffleShots g s  = item : shuffleShots g' rest
	where
	  (pos, g') = randomR (0, length s - 1) g
	  item      = s!!pos
	  rest      = delete item s

getRandomShots :: IO [(Coord)]
getRandomShots = do
	g <- newStdGen
	return (shuffleShots g shots) -}



runGame :: Interface -> IO ()
runGame i = do
  putStrLn "Welcome to the game."
  iPrintField i (iEmptyField i)
  g <- newStdGen
  let shots = iShuffleShots i g (iFullShots i)
  gameLoop i 0 (iEmptyField i) (iExampleFleet i) shots
  --gameLoop i (iShuffle i g (iFullDeck i)) (iEmpty i)

-- Play until all ships are sunk.
gameLoop :: Interface -> Int -> Field -> Fleet -> [(Coord)] -> IO ()
gameLoop i num field fleet [] = print "no shots left"
gameLoop i num field fleet shots = do
  let c = head shots
      res = iShootAtCoordinate i field c fleet
  case snd res of
  	0 -> putStrLn ("Miss")
  	1 -> putStrLn ("Hit")
  	2 -> putStrLn ("Sink")
  if (iAllShipsSunken i field)
  	then
  		finish (num + 1)
  	else
  		do
  		iPrintField i (fst res)
  		gameLoop i (num + 1) (fst res) fleet (tail shots)

-- Takes a current number of shots, a filed, a fleet and a coord where
-- the ship has been hit at
sinkShip :: Int -> Field -> Fleet -> Coord -> (Field, Int)
sinkShip shots fd ft c = undefined


finish :: Int -> IO ()
finish shots = do
  putStrLn ("The computer beat you with " ++ show shots ++ " shots")

twoRandomIntegers :: StdGen -> (Int, Int)
twoRandomIntegers g = (n1, n2)
  where
  	(n1,g1) = randomR (0,9) g
  	(n2,g2) = randomR (0,9) g1