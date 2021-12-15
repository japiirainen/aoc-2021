module Main where

import qualified AOC.Dijkstra as Dijkstra
import qualified AOC.Grid as G
import AOC.Main (pureMain)
import AOC.V2 (V2 (..), (.+.))
import qualified AOC.V2.Box as Box
import Data.Char (digitToInt, isDigit)
import qualified Data.Map as M
import Data.Maybe (mapMaybe, maybeToList)
import Data.Traversable (for)

solve :: G.Grid Int -> Either String Int
solve grid = do
  (start, end) <- case G.box grid of
    Just b -> pure (Box.bTopLeft b, Box.bBottomRight b)
    Nothing -> Left "empty grid"
  let dijkstra =
        Dijkstra.dijkstra
          ( \pos -> do
              n <- G.neighbors pos
              v <- maybeToList $ M.lookup n grid
              pure (v, n)
          )
          (== end)
          start
  case Dijkstra.dijkstraGoal dijkstra of
    Nothing -> Left "no path found"
    Just (_, _, path) ->
      pure . sum $
        mapMaybe (`M.lookup` grid) (drop 1 $ reverse path)

extend :: Int -> G.Grid Int -> Either String (G.Grid Int)
extend n grid = do
  V2 gw gh <- case G.box grid of
    Just b -> pure $ V2 (Box.width b) (Box.height b)
    Nothing -> Left "empty grid"
  pure $
    M.fromList $ do
      xg <- [0 .. n -1]
      yg <- [0 .. n -1]
      (pos, val) <- M.toList grid
      let pos' = pos .+. V2 (xg * gw) (yg * gh)
          val' = (val + xg + yg - 1) `mod` 9 + 1
      pure (pos', val')

main :: IO ()
main = pureMain $ \input -> do
  grid <- for (G.fromString input) $ \c ->
    if isDigit c then Right $ digitToInt c else Left $ "Invalid character: " ++ show c
  part1 <- solve grid
  grid' <- extend 5 grid
  part2 <- solve grid'
  pure (pure part1, pure part2)