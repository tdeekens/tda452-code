module Exercise2 where
import Test.QuickCheck

-- maxi x y returns the maximum of x and y
maxi :: Int -> Int -> Int
maxi i j | i >= j    = i
         | otherwise = j

-- sumsq n returns 1*1 + 2*2 + ... + n*n
sumsq :: Int -> Int
sumsq 0 = 0
sumsq i = (i' * i') + (sumsq (i' - 1))
   where
      i' = abs i

sumsq' :: Int -> Int
sumsq' n = n * (n+1) * (2*n + 1) `div` 6

prop_sumsq :: Int -> Bool
prop_sumsq n = sumsq' n' == sumsq n'
   where
      n' = abs n

-- fib n computes the nth Fibonacci number
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

smallestFactor :: Int -> Int
smallestFactor n = nextFactor 2 n

nextFactor :: Int -> Int -> Int
nextFactor k n | n `mod` k == 0 = k
               | otherwise      = nextFactor (k + 1) n

numFactors :: Int -> [Int]
numFactors n = [x | x <- [1..n], n `mod` x == 0]

prop_numfac :: Int -> Bool
prop_numfac n = and $ map (\x -> n `mod` x == 0) (numFactors n)

prop_smallfac :: Int -> Bool
prop_smallfac n = and $ map (\x -> n `mod` x /= 0) ps
   where
      sf = smallestFactor n
      ps = [2..sf]

multiply :: Num a => [a] -> a
multiply []     = 1
multiply (x:xs) = x * multiply xs

prop_multL :: [Int] -> Bool
prop_multL i = product i == multiply i

duplicates :: Eq a => [a] -> Bool
duplicates xs = length xs == length (removeDuplicates xs)

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates []     = []
removeDuplicates (x:xs) = case x `elem` xs of
                             True -> removeDuplicates xs
                             False -> x:removeDuplicates xs

prop_duplicatesRemoved :: [Integer] -> Bool
prop_duplicatesRemoved xs = not (duplicates (removeDuplicates xs))