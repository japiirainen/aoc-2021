{-# LANGUAGE LambdaCase #-}

module Main where

import qualified AOC.Grid as G
import AOC.Main (pureMain)
import Data.Char (digitToInt, isDigit)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Traversable (for)

step :: G.Grid Int -> (G.Grid Int, Int)
step grid0 = go 0 grid0 (M.keys grid0)
  where
    go flashes grid [] = (bound <$> grid, flashes)
    go flashes grid (p : ps) = case M.lookup p grid of
      Nothing -> go flashes grid ps
      Just v -> case compare v 9 of
        GT -> go flashes grid ps
        LT -> go flashes (M.insert p (v + 1) grid) ps
        EQ ->
          go (flashes + 1) (M.insert p (v + 1) grid) $
            ps <> G.neighbors p <> G.diagonal p

    bound x = if x > 9 then 0 else x

main :: IO ()
main = pureMain $ \input -> do
  grid <- for (G.fromString input) $ \case
    c | isDigit c -> Right $ digitToInt c
    c -> Left $ "Unknown char " ++ show c

  let grids = iterate (step . fst) (grid, 0)
      p1 = sum . map snd $ take (100 + 1) grids
      p2 =
        maybe (Left "no solution") Right $
          L.findIndex ((== M.size grid) . snd) grids

  pure (pure p1, p2)
