module Exercise3 where
import Test.QuickCheck
import Data.List

take' :: Int -> [a] -> [a]
take' 0 _              = []
take' _ []             = []
take' n (x:xs) | n > 0 = x : take' (n-1) xs
take' _ _              = error "PreludeList.take': negative argument"

drop' :: Int -> [a] -> [a]
drop' _ []             = []
drop' 0 xs             = xs
drop' n (x:xs) | n > 0 = drop' (n-1) xs

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' _ []             = ([], [])
splitAt' 0 _              = ([], [])
splitAt' n xs             = (take n xs, drop n xs)

occursIn :: Eq a => a -> [a] -> Bool
occursIn i xs = i `elem` xs

allOccursIn :: Eq a => [a] -> [a] -> Bool
allOccursIn xs ys = and $ map (\x -> x `elem` ys) xs

sameElements :: Ord a => [a] -> [a] -> Bool
sameElements xs ys = sort xs == sort ys

numOccurrences :: Eq a => a -> [a] -> Int
numOccurrences x xs = lxs
   where
      lxs = length $ elemIndices x xs

bag :: Eq a => [a] -> [(a, Int)]
bag []     = []
bag (x:xs) = (x, (length idx) + 1) : bag xs'
   where
      idx = elemIndices x xs
      xs' = deleteAll x xs

deleteAll :: Eq a => a -> [a] -> [a]
deleteAll d xs = [ x | x <- xs, x /= d ]

deleteOne :: Eq a => a -> [a] -> [a]
deleteOne _ []     = []
deleteOne e (x:xs) | x /= e    = x:deleteOne e xs
deleteOne e (x:xs) | otherwise = xs

positions :: [a] -> [(a, Int)]
positions xs = zip xs [0 .. l]
   where
      l = ((length xs) - 1)