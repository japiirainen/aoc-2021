{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import AOC.Coord (Coord (..), below, cardinal, coordCol, coordLines, manhattan)
import AOC.Main (simpleMain)
import AOC.Search (AStep (..), astar, dfs)
import Data.Char (isLetter, ord)
import Data.Map (Map)
import Data.Map qualified as Map

data Cell = Open | Amphi {amphiTarget :: !Int, amphiCost :: !Int}
  deriving (Eq, Ord, Show)

-- | Compute all the information needed from a character in the input map.
toCell :: Char -> Maybe Cell
toCell '.' = Just Open
toCell a | isLetter a = Just $! Amphi (3 + 2 * (ord a - 65)) (10 ^ (ord a - 65))
toCell _ = Nothing

-- | Predicate for rooms (rather than hallways)
isRoom :: Coord -> Bool
isRoom (C _ c) = c == 3 || c == 5 || c == 7 || c == 9

-- | Step the simulation once tracking the cost of the move.
step :: Map Coord Cell -> [AStep (Map Coord Cell)]
step w =
  [ AStep
      { astepNext = Map.insert c Open (Map.insert dest a w),
        astepCost = manhattan c dest * stepCost,
        astepHeuristic = 0
      }
    | (c, a@(Amphi target stepCost)) <- Map.toList w,
      dest <- route w c,
      if isRoom c
        then
          not (isRoom dest)
            && not (roomClean w (coordCol c))
        else
          isRoom dest
            && coordCol dest == target
            && roomClean w target
            && maybe True (a ==) (Map.lookup (below dest) w)
  ]

-- | Check that all the amphis in a room are supposed to be there.
roomClean :: Map Coord Cell -> Int -> Bool
roomClean w c = go 2
  where
    go r =
      case Map.lookup (C r c) w of
        Just Open -> go (r + 1)
        Just (Amphi t _) -> t == c && go (r + 1)
        Nothing -> True

-- | Check that everything on the map is where it should be.
done :: Map Coord Cell -> Bool
done w =
  all
    ( \(k, v) ->
        case v of
          Open -> True
          Amphi t _ -> coordCol k == t
    )
    (Map.toList w)

route :: Map Coord Cell -> Coord -> [Coord]
route w = dfs move
  where
    move c = [c' | c' <- cardinal c, Map.lookup c' w == Just Open]

main :: IO ()
main = simpleMain $ \input ->
  let inp = Map.mapMaybe toCell (Map.fromList $ coordLines (lines input))
   in (head [cost | (w, cost) <- astar step inp, done w], ())