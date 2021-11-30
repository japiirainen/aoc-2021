{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Priority queue implementation.
module AOC.PQ where

import           Data.Ord  (comparing)

import qualified Data.List as List

type Forest p v = [Tree p v] -- small to large

data Tree p v = Tree !p !v !(Forest p v)
    deriving stock Show

treeSingleton :: p -> v -> Tree p v
treeSingleton p v = Tree p v []

treePriority :: Tree p v -> p
treePriority (Tree p _ _) = p

treeValue :: Tree p v -> v
treeValue (Tree _ v _) = v

treeChildren :: Tree p v -> [Tree p v]
treeChildren (Tree _ _ cs) = cs

treeOrder :: Tree p v -> Int
treeOrder = length . treeChildren

treeMerge :: Ord p => Tree p v -> Tree p v -> Tree p v
treeMerge t1@(Tree p1 v1 c1) t2@(Tree p2 v2 c2) =
    if p1 < p2 then Tree p1 v1 (c1 <> [t2])
    else Tree p2 v2 (c2 <> [t1])

forestInsert :: Ord p => Tree p v -> Forest p v -> Forest p v
forestInsert tree = go
    where
        order = treeOrder tree

        go [] = [tree]
        go (t : ts) = case compare order (treeOrder t) of
            LT -> tree : t : ts
            GT -> t : go ts
            EQ -> forestInsert (treeMerge tree t) ts

forestMerge :: Ord p => Forest p v -> Forest p v -> Forest p v
forestMerge [] rs = rs
forestMerge ls [] = ls
forestMerge (l : ls) (r : rs) = case compare (treeOrder l) (treeOrder r) of
    LT -> l : forestMerge ls (r : rs)
    GT -> r : forestMerge (l : ls) rs
    EQ -> forestInsert (treeMerge l r) (forestMerge ls rs)

newtype PriorityQueue p v = PriorityQueue (Forest p v)
    deriving newtype Show

empty :: PriorityQueue p v
empty = PriorityQueue []

singleton :: p -> v -> PriorityQueue p v
singleton p v = PriorityQueue [treeSingleton p v]

push :: Ord p => p -> v -> PriorityQueue p v -> PriorityQueue p v
push p v (PriorityQueue f) = PriorityQueue (forestInsert (treeSingleton p v) f)

pop :: Ord p => PriorityQueue p v -> Maybe (p, v, PriorityQueue p v)
pop (PriorityQueue []) = Nothing
pop (PriorityQueue roots) =
    let ixed     = zip [0..] roots
        (idx, t) = List.minimumBy (comparing (treePriority . snd)) ixed
        remain   = let (pre, post) = splitAt idx roots in pre <> drop 1 post
        roots'   = forestMerge remain (treeChildren t) in
    Just (treePriority t, treeValue t, PriorityQueue roots')
