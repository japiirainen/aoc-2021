{-# LANGUAGE TypeApplications #-}
module Main where

import qualified AOC.Parsija   as P
import           Control.Arrow ((&&&))

solve :: (Int -> Int) -> [Int] -> Int
solve trans ps = minimum $ (\p -> calculateCosts trans p ps) <$> [minB..maxB]
    where (minB, maxB) = (minimum &&& maximum) ps

calculateCosts :: (Int -> Int) -> Int -> [Int] -> Int
calculateCosts trans pos = sum . map (trans . abs . (pos -))

incWithDist :: Int -> Int
incWithDist x = x * (x + 1) `div` 2

main :: IO ()
main = do
    ip <- readFile "2021/07.txt"
    case P.runParser (P.decimal @Int `P.sepBy1` P.char ',') ip of
        Left err -> print err
        Right xs -> do
            print $ "Part 1: " <> show (solve id xs)
            print $ "Part 2: " <> show (solve incWithDist xs)
