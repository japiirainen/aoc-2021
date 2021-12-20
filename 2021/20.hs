module Main where

import AOC hiding (parseInput)
import AOC.Main (pureMain)
import Data.Map qualified as M
import Data.Set qualified as S

parseInput :: Parser (Set Int, Set Coords, Integer, Integer)
parseInput = do
  alg <- M.fromList . zip [0 ..] <$> many printChar <* newline <* newline
  (grid, width, height) <- makeGrid' <$> takeRest
  pure (toSet alg, toSet grid, width, height)
  where
    toSet = mapToSet (== '#')

main :: IO ()
main = pureMain $ \input -> do
  (alg, grid, width, height) <- parseE parseInput input
  let isLit = memo2 \case
        0 -> (`S.member` grid)
        n -> \(x, y) -> fromBits [isLit (n - 1) (x + j, y + i) | i <- [-1 .. 1], j <- [-1 .. 1]] `S.member` alg
  rs <- for [2, 50] \n ->
    Right $ howMany @Int (isLit n) $ (,) <$> [- n .. width + n -1] <*> [- n .. height + n -1]
  pure (pure $ head rs, pure $ rs !! 1)
