module Main where

import           AOC.Main            (pureMain)
import qualified AOC.Parsija         as P
import           AOC.V2              (V2 (..))
import           Control.Applicative (many)
import qualified Data.Map            as M

data LN = LN (V2 Int) (V2 Int)

parseInput :: P.Parser Char [LN]
parseInput = many $ LN <$> (v2 <* P.string "->" <* P.spaces) <*> v2
  where v2 = V2 <$> (P.decimal <* P.char ',') <*> (P.decimal <* P.spaces)

points :: LN -> [V2 Int]
points (LN (V2 x0 y0) (V2 x1 y1))
    | x0 == x1  = [V2 x0 y | y <- range y0 y1]
    | y0 == y1  = [V2 x y0 | x <- range x0 x1]
    | otherwise = zipWith V2 (range x0 x1) (range y0 y1)
  where range n k = if n <= k then [n .. k] else [n, n - 1 .. k]

isStraight :: LN -> Bool
isStraight (LN (V2 x1 y1) (V2 x2 y2)) = x1 == x2 || y1 == y2

overlaps :: [LN] -> Int
overlaps ls = M.size . M.filter (>= 2) $
    M.fromListWith (+) [(p, 1 :: Int) | l <- ls, p <- points l]

main :: IO ()
main = pureMain $ \input -> do
  ip <- P.runParser parseInput input
  pure (pure (overlaps $ filter isStraight ip), pure (overlaps ip))
