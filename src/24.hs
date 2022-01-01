module Main where

import AOC (chunksOf, intToDigit)
import AOC.Main (simpleMain)

impl :: Int -> Int -> Int -> Int -> Int -> Int
impl a b c w z
  | z `mod` 26 + b == w = z `div` a
  | otherwise = z `div` a * 26 + w + c

solve :: [Int] -> Int -> [(Int, Int, Int)] -> [[Int]]
solve guesses !z ((a, b, c) : bs) =
  [ i : is
    | i <- if b < 0 then [w | let w = z `mod` 26 + b, 1 <= w, w <= 9] else guesses,
      is <- solve guesses (impl a b c i z) bs
  ]
solve _ z [] = [[] | z == 0]

main :: IO ()
main = simpleMain $ \input ->
  let inp = map extract $ chunksOf 18 $ map words $ lines input
      p1 = map intToDigit $ head $ solve [9, 8 .. 1] 0 inp
      p2 = map intToDigit $ head $ solve [1, 2 .. 9] 0 inp
   in (p1, p2)

extract :: [[String]] -> (Int, Int, Int)
extract
  [ ["inp", "w"],
    ["mul", "x", "0"],
    ["add", "x", "z"],
    ["mod", "x", "26"],
    ["div", "z", a],
    ["add", "x", b],
    ["eql", "x", "w"],
    ["eql", "x", "0"],
    ["mul", "y", "0"],
    ["add", "y", "25"],
    ["mul", "y", "x"],
    ["add", "y", "1"],
    ["mul", "z", "y"],
    ["mul", "y", "0"],
    ["add", "y", "w"],
    ["add", "y", c],
    ["mul", "y", "x"],
    ["add", "z", "y"]
    ] =
    (read a, read b, read c)
extract x = error ("invalid input: " <> show x)