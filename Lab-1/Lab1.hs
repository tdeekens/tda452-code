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

-- Part 1 - Steps for power

steps :: Integer -> Integer -> Integer
steps n k | k < 0     = error "power: negative argument"
          | k == 0    = 1
          | otherwise = 1 + steps n (k-1)

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

-- Part 3

power2 :: Integer -> Integer -> Integer
power2 n k | k < 0     = error "power: negative argument"
           | k == 0    = 1
           | odd k     = n * power2 n (k-1)
           | otherwise = power2 (n * n) (k `div` 2)

{-
   # Part 4 a.

   ## Test cases

   - (1, 0) the lowest allowed value should return 1
      - Needs to be covered as it is the base case and does
        not enter any recursion etc.
   - (1, -1) an unallowed value as power to a negative number
     can not be calculated using our implementations
   - (5, 2) a sane and easy value to prove that it works
     as expected and correctly
   - (-5, 2) testing a negative n whereby the result should
     be positive with an even k
   - (-5, 3) testing a negative n whereby the result should
     be negative with an odd k

   ## Defined inputs

   - Negative Integers which are not allowed for the power value
     are caught by the functions pattern matching
   - N and k must be of type Integer. For other
     types (e.g. Floats) the function can currently be not be applied
-}

-- Part 4 b.

-- Property comparing all posible combinations of
-- power, power1 and power2

prop_power n k = let k' = abs k in
                 power n k' == power1 n k'
                 && power1 n k' == power2 n k'
                 && power2 n k' == power n k'

-- Part 4 c.

-- Define function to get first...
first :: (a, b) -> a
first (a, b) = a

-- ... and second item of a tuple
second :: (a, b) -> b
second (a, b) = b

-- Define a list with tuples holding test cases as defined in 4 a
testCases :: [(Integer, Integer)]
testCases = [(1, 0), (1, -1), (5, 2), (-5, 2), (-5, 3)]

-- Define a function which takes a list of tuples and
-- checks the property defined in 4 b.
runTests :: [(Integer, Integer)] -> Bool
runTests [] = True
runTests (x:xs) = (prop_power (first x) (second x)) && runTests xs

-- Part 4 d.

-- running `quickCheck prop_power` in the prompt passes 100 tests