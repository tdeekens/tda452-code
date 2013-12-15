module Exercise4 where
import Test.QuickCheck
import Data.List
import Data.Maybe

{--
readN :: FilePath -> Int -> IO (Int)
readN fp n = do
               c <- readFile fp
               let ls = lines c
                   ps = dropWhile (<=0) ls
                   p  = head ps
                   ns = take n (drop n ps)
                   s  = (sum ns) + p
               return (s)

readPositive :: FilePath -> IO ()
readPositive fp = do
                     c <- readFile fp
                     let ls  = lines c
                         ps  = takeWhile (>0) ls
                         pss = sort ps
                     print ps
--}

look' :: Eq a => a -> [(a,b)] -> Maybe b
look' x []           = Nothing
look' x ((x',y):xys)
  | x == x'         = Just y
  | otherwise       = look' x xys

prop_look_look :: Eq a => Eq b => a -> [(a,b)] -> Bool
prop_look_look a tls = look' a tls == lookup a tls

prop_LookNothing :: Eq a => a -> [(a,b)] -> Property
prop_LookNothing a tls = isNothing (lookup a tls) ==> not (elem a [fst x | x <- tls])

prop_LookJust :: Eq a => a -> [(a,b)] -> Property
prop_LookJust a tls = isJust looked ==> (elem a [fst x | x <- tls])
   where
      looked  = lookup a tls
      looked' = Just looked

prop_Look :: Eq a => a -> [(a,b)] -> Property
prop_Look x xs = (prop_LookJust x xs) .||. (prop_LookNothing x xs)

sequence' :: [IO a] -> IO ()
sequence' []     = return ()
sequence' (i:is) =
  do i
     sequence' is

onlyIf :: Bool -> IO () -> IO ()
onlyIf False _  = return ()
onlyIf True  op = do op

listOf' :: Integer -> Gen a -> Gen [a]
listOf' 0 _ = return []
listOf' n g = do
               i <- arbitrary
               return (i : listOf' (n - 1) g)

prop_listOf :: Integer -> Gen a -> Bool
prop_listOf n g = (length l) == fromInteger n
   where
      l = listOf' n g

ordered :: Eq a => [a] -> Bool
ordered []                      = True
ordered (x:xs) | x <= (head xs) = ordered xs
ordered _      | otherwise      = False

