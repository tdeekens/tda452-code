module SliceNDice where

-- sample data
nested  = [ [1..6], [7..12], [13..18] ]
nested2 = [ [1..6], [7..12], [13..18], [19..24] ]
list    = [1..10]

-- slices a part of a list starting at `from` with `size`
slice' :: Int -> Int -> [a] -> [a]
slice' from size = take size . drop from

-- slices a part of a list starting at `from` with `size`
-- returning a tuple with the `slice` and the `rest`
slice'' :: Int -> Int -> [a] -> ([a], [a])
slice'' from size l = (head', tail')
   where
      head' = slice' from size l
      tail' = drop (from + size) l

-- THIS WAS A BIT FUBAR!
-- chops a list of lists in slice of `size` starting at `op`
-- giving a list for each vertial slice through the lists
chunks' :: [[a]] -> Int -> [[a]]
chunks' [[],[],[]] _ = []
chunks' ls         s = [heads'] ++ (chunks' tails' s)
   where
      -- list of tuples in [(head, tail)...]
      chopinception = map (slice'' 0 s) ls
      heads'        = concat $ map fst chopinception
      tails'        = map snd chopinception

chunks'' :: [[a]] -> Int -> [[a]]
chunks'' ls _ | null $ filter (not . null) ls = []
chunks'' ls s | otherwise                     = [heads'] ++ (chunks'' tails' s)
   where
      -- list of tuples in [(head, tail)...]
      chopinception = map (slice'' 0 s) ls
      heads'        = concat $ map fst chopinception
      tails'        = map snd chopinception