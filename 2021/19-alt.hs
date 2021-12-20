{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import AOC.Main (pureMain)
import Data.Either (partitionEithers)
import Data.List (nub, transpose)
import Data.List.Split (splitOn)
import Data.Map qualified as M

parse :: Read b => String -> [[[b]]]
parse = map (map parsePos . tail) . splitOn [""] . lines

parsePos :: Read b => [Char] -> [b]
parsePos = map read . splitOn ","

type Pos = [Int]

type Scanner = [Pos]

align :: [(Scanner, Pos)] -> [Scanner] -> [Scanner] -> [(Scanner, Pos)]
align result _ [] = result
align result (ref : refs) scanners =
  align (found <> result) (map fst found <> refs) notFound
  where
    (found, notFound) =
      partitionEithers
        [ maybe (Right scanner) Left . safeHead $ align2 ref scanner
          | scanner <- scanners
        ]

align2 :: Scanner -> Scanner -> [(Scanner, Pos)]
align2 a b = [(map (add pos) o, pos) | o <- orientations b, pos <- overlap a o]

safeHead :: [a] -> Maybe a
safeHead = \case
  [] -> Nothing
  (x : _) -> Just x

overlap :: Scanner -> Scanner -> [Pos]
overlap as bs =
  M.keys . M.filter (>= (12 :: Int)) . M.fromListWith (+) . map (,1) $
    diff <$> as <*> bs

diff :: Pos -> Pos -> Pos
diff = zipWith (-)

add :: Pos -> Pos -> Pos
add = zipWith (+)

orientations :: Scanner -> [Scanner]
orientations ps = transpose $ map orientationsPos ps

orientationsPos :: Pos -> [Pos]
orientationsPos p = scanl (flip ($)) p steps
  where
    steps = [r, t, t, t, r, t, t, t, r, t, t, t, r . t . r . r, t, t, t, r, t, t, t, r, t, t, t]
    r [x, y, z] = [x, z, - y]
    t [x, y, z] = [- y, x, z]

pick2 :: [a] -> [(a, a)]
pick2 [] = []
pick2 (x : xs) = map (x,) xs <> pick2 xs

manhattan :: Pos -> Pos -> Int
manhattan a b = sum $ map abs $ diff a b

solve :: [Scanner] -> Either String (Int, Int)
solve [] = Left "no scanners provided"
solve (x : xs) = Right (part1, part2)
  where
    aligned = align [(x, [0, 0, 0])] [x] xs
    part1 = length . nub . concat . map fst $ aligned
    part2 = maximum . map (uncurry manhattan) . pick2 . map snd $ aligned

main :: IO ()
main = pureMain $ \input -> do
  (p1, p2) <- solve $ parse input
  pure (pure p1, pure p2)
