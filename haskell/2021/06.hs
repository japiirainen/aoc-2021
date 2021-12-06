{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import qualified AOC.Parsija as P
import           Data.List


initC :: [Int] -> [Int]
initC = map (pred . length) . group . sort . (++ [0 .. 8])

step :: [Int] -> [Int]
step (zeros:xs) = prefix <> [sevens + zeros] <> [eights, zeros]
    where (prefix, [sevens, eights]) = splitAt 6 xs

solve :: Int -> [Int] -> Int
solve n = sum . (!! n) . iterate step . initC

main :: IO ()
main = do
    ip <- readFile "2021/06.txt"
    case P.runParser (P.sepBy1 P.decimal (P.char ',') <* P.spaces) ip of
        Left err -> print err
        Right xs -> do
            print $ "Part 1: " <> show (solve 80 xs)
            print $ "Part 2: " <> show (solve 256 xs)
