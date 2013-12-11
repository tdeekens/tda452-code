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
  , iAddToFleet :: Fleet -> Boat -> (Fleet, Bool)
  , iIsValidFleet :: Fleet -> Bool
  , iSinkShip :: [Direction] -> Field -> Fleet -> Coord -> [Coord] -> (Field, [Coord])
  , iDirections :: [Direction]
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

positionFleet :: Interface -> IO ()
positionFleet i = do
	putStrLn "Lets construct the fleet!"
	addBoats i 0 (iEmptyFleet i)

addBoats :: Interface -> Int -> Fleet -> IO()
addBoats i pos f = do
	let b   = (bs!!pos)
	let res = iAddToFleet i f b
	case (snd res) of
            False -> addBoats i (pos+1) f
            True  -> if (iIsValidFleet i (fst res))
					then
						do
							let fleet = fst res
							putStrLn "Got the fleet!"
							print fleet
							runGame' i fleet
					else
						addBoats i (pos+1) (fst res)

runGame :: Interface -> IO ()
runGame i = do
  putStrLn "Welcome to the game."
  iPrintField i (iEmptyField i)
  g <- newStdGen
  let shots = iShuffleShots i g (iFullShots i)
  gameLoop i 0 (iEmptyField i) (iExampleFleet i) shots
  --gameLoop i (iShuffle i g (iFullDeck i)) (iEmpty i)

-- Takes a fleet as a parameter
runGame' :: Interface -> Fleet -> IO ()
runGame' i f = do
  putStrLn "Welcome to the game."
  iPrintField i (iEmptyField i)
  g <- newStdGen
  let shots = iShuffleShots i g (iFullShots i)
  gameLoop i 0 (iEmptyField i) f shots
{-
-- Play until all ships are sunk.
gameLoop :: Interface -> Int -> Field -> Fleet -> [(Coord)] -> IO ()
gameLoop i num field fleet [] = print "no shots left"
gameLoop i num field fleet shots = do
  let c = head shots
      res = iShootAtCoordinate i field c fleet
  case snd res of
  	0 -> putStrLn ("Miss")
  	1 -> do
           putStrLn ("Hit")
  		   iPrintField i (fst res)
  		   let sinkRes = iSinkShip i (iDirections i) (fst res) fleet c (tail shots)
  		   print shots
  		   putStrLn ("Sink")
  		   iPrintField i (fst sinkRes)
  		   gameLoop i (num + 1) (fst sinkRes) fleet (snd sinkRes)
  	2 -> putStrLn ("Sink")
  if (iAllShipsSunken i (fst res))
  	then
  		finish (num + 1)
  	else
  		do
          print shots
          iPrintField i (fst res)
          if ((snd res) == 0 || (snd res)==2)
            then
              gameLoop i (num + 1) (fst res) fleet (tail shots)
            else
              finish (num + 1)
-}


gameLoop :: Interface -> Int -> Field -> Fleet -> [(Coord)] -> IO ()
gameLoop i num field fleet [] = print "no shots left"
gameLoop i num field fleet shots = do
    if (iAllShipsSunken i field)
    then
       finish num
  	else
        do
          print shots
          let c = head shots
              res = iShootAtCoordinate i field c fleet
          case snd res of
            0 -> do
                   putStrLn ("Miss")
                   iPrintField i (fst res)
                   gameLoop i (num + 1) (fst res) fleet (tail shots)
            1 -> do
                   putStrLn ("Hit")
                   iPrintField i (fst res)
                   let sinkRes = iSinkShip i (iDirections i) (fst res) fleet c (tail shots)
                   print shots
                   putStrLn ("Sink")
                   iPrintField i (fst sinkRes)
                   let us = (length shots) - (length (snd sinkRes))
                   gameLoop i (num + us) (fst sinkRes) fleet (snd sinkRes)
            2 -> do
            	   putStrLn ("Sink")
                   iPrintField i (fst res)
                   gameLoop i (num + 1) (fst res) fleet (tail shots)

finish :: Int -> IO ()
finish shots = do
  putStrLn ("The computer beat you with " ++ show shots ++ " shots")

twoRandomIntegers :: StdGen -> (Int, Int)
twoRandomIntegers g = (n1, n2)
  where
  	(n1,g1) = randomR (0,9) g
  	(n2,g2) = randomR (0,9) g1

bs :: [Boat]
bs = [Boat {model = Submarine, start = (5,1), alignment = Vertical}
	, Boat {model = Destroyer, start = (1,4), alignment = Vertical}
	, Boat {model = AircraftCarrier, start = (3,2), alignment = Horizontal}
	, Boat {model = Destroyer, start = (2,5), alignment = Horizontal}
	, Boat {model = Submarine, start = (5,0), alignment = Horizontal}
	, Boat {model = Battleship, start = (8,9), alignment = Vertical}
	, Boat {model = Battleship, start = (0,2), alignment = Vertical}
	, Boat {model = Battleship, start = (2,3), alignment = Vertical}
	, Boat {model = AircraftCarrier, start = (3,9), alignment = Horizontal}
	, Boat {model = PatrolBoat, start = (7,5), alignment = Horizontal}
	, Boat {model = Battleship, start = (4,0), alignment = Horizontal}
	, Boat {model = Battleship, start = (7,6), alignment = Horizontal}
	, Boat {model = PatrolBoat, start = (6,4), alignment = Vertical}
	, Boat {model = Destroyer, start = (2,2), alignment = Horizontal}
	, Boat {model = AircraftCarrier, start = (0,0), alignment = Vertical}
	, Boat {model = Destroyer, start = (8,8), alignment = Horizontal}
	, Boat {model = Battleship, start = (8,7), alignment = Vertical}
	, Boat {model = Submarine, start = (8,5), alignment = Horizontal}
	, Boat {model = Destroyer, start = (5,8), alignment = Horizontal}
	, Boat {model = PatrolBoat, start = (8,4), alignment = Horizontal}
	, Boat {model = Destroyer, start = (3,0), alignment = Horizontal}
	, Boat {model = PatrolBoat, start = (9,0), alignment = Horizontal}
	, Boat {model = PatrolBoat, start = (2,0), alignment = Horizontal}
	, Boat {model = Submarine, start = (7,5), alignment = Horizontal}
	, Boat {model = Battleship, start = (3,5), alignment = Vertical}
	, Boat {model = PatrolBoat, start = (3,3), alignment = Vertical}
	, Boat {model = Submarine, start = (6,2), alignment = Vertical}
	, Boat {model = PatrolBoat, start = (2,0), alignment = Vertical}
	, Boat {model = PatrolBoat, start = (7,0), alignment = Vertical}
	, Boat {model = PatrolBoat, start = (8,1), alignment = Vertical}
	, Boat {model = Submarine, start = (9,2), alignment = Horizontal}
	, Boat {model = AircraftCarrier, start = (1,7), alignment = Vertical}
	, Boat {model = Battleship, start = (5,6), alignment = Horizontal}
	]