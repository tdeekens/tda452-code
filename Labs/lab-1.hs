module Lab1 where
import Test.QuickCheck

{-
   Lab Week 1
   Anna Averianova & Tobias Deekens, 2013
-}

-- Power as given in Lab paper

power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

{-
   # Part 1

   - Steps for power: k + 1
-}

-- Part 2 - Using replicate

power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power: negative argument"
power1 n k = product l
   where l = replicate (fromInteger k)  (fromInteger n)

-- Part 2 - Using list comprehensions

power1_2 :: Integer -> Integer -> Integer
power1_2 n k | k < 0 = error "power: negative argument"
power1_2 n k = product l
   where l = [n | _ <- [1..k]]