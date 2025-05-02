-- A pure function in Haskell
sum [1, 2, 3, 4]
-- We can substitute equals for equals
sum [1, 2] + sum [3, 4]
-- Or even
sum (map (\x -> x) [1, 2, 3, 4])
