module AOC.Dijkstra where

import           Control.Monad (guard)
import           Data.Map      (Map)

import qualified AOC.PQ        as PQ
import qualified Data.List     as List
import qualified Data.Map      as Map

data Bfs v = Bfs
    { bfsDistances :: Map v [v]
    , bfsGoal      :: Maybe (v, [v])
    }

bfs :: Ord v
    => (v -> [v])
    -- ^ Neighbors
    -> (v -> Bool)
    -- ^ Is this a goal?
    -> v
    -- ^ Start
    -> Bfs v
bfs neighbors goal start = go Map.empty (Map.singleton start [start])
    where
        go visited fringe
            | Map.null fringe = Bfs visited Nothing
            | g : _ <- goals = Bfs visited' (Just g)
            | otherwise = go visited' fringe'
          where
              goals = filter (goal . fst) (Map.toList fringe)
              visited' = Map.unionWith const visited fringe
              fringe' = Map.fromList $ do
                  (n, path) <- Map.toList fringe
                  nb <- neighbors n
                  guard . not $ nb `Map.member` visited'
                  pure (nb, nb : path)

data Dijkstra v = Dijkstra
    { dijkstraDistances :: Map v (Int, [v])
    , dijkstraGoal      :: Maybe (v, Int, [v])
    }

dijkstra :: Ord v
         => (v -> [(Int, v)])
         -- ^ Neighbors
         -> (v -> Bool)
         -- ^ Is this a goal?
         -> v
         -- ^ Start
         -> Dijkstra v
dijkstra neighbors goal start = go (PQ.singleton 0 (start, [start])) Map.empty
    where
        go fringe0 dist0 = case PQ.pop fringe0 of
            Nothing -> Dijkstra dist0 Nothing
            Just (cdist, (current, path), fringe1)
                | current `Map.member` dist0 -> go fringe1 dist0
                | otherwise ->
                    let interesting = do
                            (d, neighbor) <- neighbors current
                            guard $ not $ neighbor `Map.member` dist0
                            pure (cdist + d, neighbor)

                        dists1 = Map.insert current (cdist, path) dist0

                        fringe2 = List.foldl'
                            (\acc (d, v) -> PQ.push d (v, v : path) acc)
                            fringe1 interesting in
                    if goal current
                        then Dijkstra dists1 $ Just (current, cdist, path)
                        else go fringe2 dists1

