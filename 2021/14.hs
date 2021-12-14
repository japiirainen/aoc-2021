{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

import AOC.Main (pureMain)
import qualified AOC.Parsija as P
import Data.Foldable (foldl')
import qualified Data.Map as M
import Data.Monoid (Endo (..))
import Data.Semigroup (stimes)

data Pair a = Pair a a deriving (Eq, Foldable, Functor, Ord, Show)

type Rules a = M.Map (Pair a) a

newtype Freqs a = Freqs {unFreqs :: M.Map a Integer} deriving (Show)

times :: Integer -> a -> Freqs a
times n x = Freqs $ M.singleton x n

instance Ord a => Semigroup (Freqs a) where
  Freqs a <> Freqs b = Freqs $ M.unionWith (+) a b

instance Ord a => Monoid (Freqs a) where mempty = Freqs M.empty

separate :: (Foldable f, Ord a) => Freqs (f a) -> Freqs a
separate = M.foldMapWithKey (\f n -> foldMap (times n) f) . unFreqs

initial :: Ord a => [a] -> Freqs (Pair a)
initial l = mconcat $ zipWith (\x y -> times 1 (Pair x y)) l (drop 1 l)

step :: Ord a => Rules a -> Freqs (Pair a) -> Freqs (Pair a)
step rules (Freqs m) =
  M.foldMapWithKey
    ( \pair@(Pair x y) n -> case M.lookup pair rules of
        Nothing -> times n pair
        Just z -> times n (Pair x z) <> times n (Pair z y)
    )
    m

solve :: Ord a => Rules a -> [a] -> Int -> Integer
solve rules polymer n =
  let steps = appEndo . stimes n . Endo $ step rules
      freqs =
        fmap (`div` 2) . unFreqs $
          times 1 (head polymer) <> times 1 (last polymer)
            <> separate (steps $ initial polymer)
   in maximum freqs - minimum freqs

parseInput :: P.Parser Char (String, Rules Char)
parseInput =
  (,)
    <$> P.many1 P.upper <* P.spaces
    <*> (foldl' insertRule M.empty <$> P.many1 parseRule)
  where
    insertRule acc (x, y, z) = M.insert (Pair x y) z acc
    parseRule =
      (,,)
        <$> P.upper
        <*> P.upper <* P.spaces <* P.string "->" <* P.spaces
        <*> P.upper <* P.spaces

main :: IO ()
main = pureMain $ \input -> do
  (polymer, rules) <- P.runParser parseInput input
  let p1 = solve rules polymer 10
      p2 = solve rules polymer 40
  pure (pure p1, pure p2)
