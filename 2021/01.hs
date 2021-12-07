module Main where

import           AOC.Main      (simpleMain)
import           Data.Foldable (foldl')

countIncreasing :: [Int] -> Int
countIncreasing = snd . foldl' (\(prev, count) x -> if x > prev then (x, count + 1) else (x, count)) (maxBound, 0)

part2 :: [Int] -> Int
part2 = countIncreasing . map sum . sliding 3

sliding :: Int -> [a] -> [[a]]
sliding n xs
    | length xs < n = []
    | otherwise = take n xs : sliding n (tail xs)

main :: IO ()
main = simpleMain $ \input ->
  let numbers = map read $ lines input
  in (countIncreasing numbers, part2 numbers)
