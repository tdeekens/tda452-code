module Test where

import DataTypes
import Data.Maybe
import Data.Char
import Data.List

finish :: Int -> IO ()
finish shots = putStrLn s
  where
    s' = (show shots)
    s  = "The computer beat you with " ++ s' ++ " shots!"