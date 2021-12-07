{-# LANGUAGE TypeApplications #-}
module Main where

import           AOC.Main      (pureMain)
import           Control.Arrow ((&&&))

import qualified AOC.Parsija   as P

solve :: (Int -> Int) -> [Int] -> Int
solve trans ps = minimum $ (\p -> calculateCosts trans p ps) <$> [minB..maxB]
    where (minB, maxB) = (minimum &&& maximum) ps

calculateCosts :: (Int -> Int) -> Int -> [Int] -> Int
calculateCosts trans pos = sum . map (trans . abs . (pos -))

incWithDist :: Int -> Int
incWithDist x = x * (x + 1) `div` 2

main :: IO ()
main = pureMain $ \input -> do
  xs <- P.runParser (P.decimal `P.sepBy1` P.char ',') input
  pure (pure (solve id xs), pure (solve incWithDist xs))
