module AOC.Foldable where

import Control.Lens (folded, foldlOf')
import qualified Control.Lens as Control.Lens.Getter
import qualified Control.Monad.RWS as Data.Semigroup.Internal
import qualified Data.Foldable as F
import Data.Ord (comparing)

minimaBy :: Foldable t => (a -> a -> Ordering) -> t a -> [a]
minimaBy f = F.foldl' step []
  where
    step [] x = [x]
    step (y : ys) x = case f x y of
      EQ -> x : y : ys
      LT -> [x]
      GT -> y : ys

maximaBy :: (Ord a, Foldable t) => (a -> a -> Ordering) -> t a -> [a]
maximaBy f = minimaBy (\x y -> down (f x y))
  where
    down LT = GT
    down EQ = EQ
    down GT = LT

minimumOn :: (Ord b, Foldable t) => (a -> b) -> t a -> a
minimumOn = F.minimumBy . comparing

maximumOn :: (Ord b, Foldable t) => (a -> b) -> t a -> a
maximumOn = F.maximumBy . comparing

howMany :: (Num n, Foldable t) => (a -> Bool) -> t a -> n
howMany = howManyOf folded

howManyOf ::
  Num a =>
  Control.Lens.Getter.Getting
    ( Data.Semigroup.Internal.Endo
        (Data.Semigroup.Internal.Endo a)
    )
    s
    t ->
  (t -> Bool) ->
  s ->
  a
howManyOf t p = foldlOf' t (\c e -> if p e then c + 1 else c) 0