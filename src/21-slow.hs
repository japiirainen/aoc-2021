module Main where

import AOC (MonadParsec (eof), Parser, chunksOf, counts, decimal, newline, parseE, replicateM, (.*))
import AOC.Main (pureMain)
import Data.Map.Strict qualified as M

parse :: Parser (Int, Int)
parse =
  (,) <$ "Player 1 starting position: " <*> decimal <* newline
    <* "Player 2 starting position: " <*> decimal
    <* eof

mod1 :: Int -> Int -> Int
mod1 x n = (x - 1) `mod` n + 1

part1 :: Int -> Int -> Int
part1 = go deterministic 0 0 0
  where
    deterministic = sum <$> chunksOf 3 (cycle [1 .. 100])
    go [] _ _ _ _ _ = 0
    go (d : ds) !n !s1 !s2 !p1 !p2
      | s2 >= 1000 = s1 * n
      | otherwise = go ds (n + 3) s2 (s1 + p1') p2 p1'
      where
        p1' = (p1 + d) `mod1` 10

part2 :: Int -> Int -> Int
part2 = maximum . M.fromListWith (+) .* go 1 True 0 0
  where
    dirac = M.assocs . counts $ sum <$> replicateM 3 [1 .. 3]
    go !c !p !s1 !s2 !p1 !p2
      | s2 >= 21 = pure (p, c)
      | otherwise = do
        (d, c') <- dirac
        let p1' = (p1 + d) `mod1` 10
        go (c * c') (not p) s2 (s1 + p1') p2 p1'

main :: IO ()
main = pureMain $ \input -> do
  (p1, p2) <- parseE parse input
  pure (pure (part1 p1 p2), pure (part2 p1 p2))
