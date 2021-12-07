{-# LANGUAGE TypeApplications #-}

import qualified AOC.Parsija   as P
import           Control.Arrow ((&&&))


main :: IO ()
main = do
    ip <- readFile "2021/07.txt"
    case P.runParser (P.decimal @Int `P.sepBy` P.char ',') ip of
        Left err -> print err
        Right positions -> do
            print $ "Part 1: " <> show (solve id positions)
            print $ "Part 2: " <> show (solve increaseWithDistance positions)


increaseWithDistance :: Int -> Int
increaseWithDistance x = x * (x + 1) `div` 2

solve :: (Int -> Int) -> [Int] -> Int
solve distToFuel positions = minimum [cost position distToFuel positions | let (minB, maxB) = minMax positions, position <- [minB .. maxB]]

minMax :: [Int] -> (Int, Int)
minMax = minimum &&& maximum

cost :: Int -> (Int -> Int) -> [Int] -> Int
cost position distToFuel = sum . map (distToFuel . abs . (position -))
