{-# LANGUAGE TypeApplications #-}
module Main where

import qualified AOC.Parsija   as P
import           Control.Arrow ((&&&))

minMax :: [Int] -> (Int, Int)
minMax = minimum &&& maximum

solve :: (Int -> Int) -> [Int] -> Int
solve trans ps = minimum [calculateCosts trans pos ps | let (minB, maxB) = minMax ps, pos <- [minB..maxB]]

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
            let sol1 = solve id xs
                sol2 = solve incWithDist xs
            print $ "Part 1: " <> show sol1
            print $ "Part 2: " <> show sol2
