module AOC.List.Extended where


select :: [a] -> [(a, [a])]
select []     = []
select (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- select xs]
