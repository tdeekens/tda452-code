module Wrapper where

import Data.Char
import Data.List
import Data.Maybe
import System.Random
import DataTypes
import FleetParser

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
  , iPrintFleet :: Fleet -> IO ()
  }

startGame :: Interface -> IO ()
startGame i = do
	putStrLn "Welcome to the game of Battleship!"
	putStrLn "Please provide a name of a file containing a fleet"
	fp <- getLine
	contents <- readFile fp
	let f = pFleet contents
        if (isJust f)
        then
          if (iIsValidFleet i (fst(fromJust f)))
          then do 
          	putStrLn "This is the fleet"
          	iPrintFleet i (fst(fromJust f))
          	putStrLn "Computer will shoot you now..."
          	runGame i (fst(fromJust f))
          else do putStrLn "The fleet in the file is not valid. Try another one? y/n"
                  answ <- getLine
                  case answ of
                    "y" -> startGame i
                    "n" -> putStrLn "Bye bye!"
        else do
        	putStrLn "Could not read the fleet from a file. Try another one? y/n"
        	answ <- getLine
        	case answ of
        		"y" -> startGame i
        		"n" -> putStrLn "Bye bye!"

-- Takes a fleet as a parameter and starts the game
runGame :: Interface -> Fleet -> IO ()
runGame i f = do
  putStrLn "*** The initial field***"
  iPrintField i (iEmptyField i)
  g <- newStdGen
  putStrLn "*** Go! ***"
  let shots = iShuffleShots i g (iFullShots i)
  gameLoop i 0 (iEmptyField i) f shots

gameLoop :: Interface -> Int -> Field -> Fleet -> [(Coord)] -> IO ()
gameLoop i num field fleet [] = print "No shots left"
gameLoop i num field fleet shots = do
    if (iAllShipsSunken i field)
    then
       finish num
  	else
        do
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