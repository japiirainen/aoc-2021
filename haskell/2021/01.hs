module Main where

import           Data.Foldable (foldl')

countIncreasing :: [Int] -> Int
countIncreasing = snd . foldl' (\(prev, count) x -> if x > prev then (x, count + 1) else (x, count)) (maxBound, 0)

part2 :: [Int] -> Int
part2 = countIncreasing . map sum . sliding 3

main :: IO ()
main = do
    numbers <- map read . lines <$> readFile "2021/01.txt" :: IO [Int]
    let r1 = countIncreasing numbers
        r2 = part2 numbers
    print $ "Part 1 result: " <> show r1
    print $ "Part 2 result: " <> show r2
    pure ()

sliding :: Int -> [a] -> [[a]]
sliding n xs
    | length xs < n = []
    | otherwise = take n xs : sliding n (tail xs)
