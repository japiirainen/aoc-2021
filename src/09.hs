{-# LANGUAGE LambdaCase #-}

module Main where

import AOC.Dijkstra (Bfs (..), bfs)
import qualified AOC.Grid as G
import AOC.Main (pureMain)
import Control.Monad (guard)
import Data.Char (digitToInt, isDigit)
import Data.List (sortOn)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Data.Ord (Down (..))
import Data.Traversable (for)

lowPoints :: Ord a => G.Grid a -> [(G.Pos, a)]
lowPoints grid = do
  (pos, height) <- M.toList grid
  guard . and $ do
    n <- G.neighbors pos
    h <- maybeToList $ M.lookup n grid
    pure $ h > height
  pure (pos, height)

basin :: Ord a => G.Grid a -> G.Pos -> [G.Pos]
basin grid = M.keys . bfsDistances . bfs neighbors (const False)
  where
    neighbors :: G.Pos -> [G.Pos]
    neighbors pos = do
      height <- maybeToList $ M.lookup pos grid
      n <- G.neighbors pos
      h <- maybeToList $ M.lookup n grid
      guard $ h > height
      pure n

main :: IO ()
main = pureMain $ \input -> do
  grid <- for (G.fromString input) $ \case
    c | isDigit c -> Right $ digitToInt c
    c -> Left $ "Unknown char " ++ show c

  let lps = lowPoints grid
      p1 = sum $ map (succ . snd) lps
      grid' = M.filter (< 9) grid
      basins = map (basin grid' . fst) lps
      p2 = product . take 3 $ sortOn Down (map length basins)

  pure (pure p1, pure p2)
