module Main where

import AOC.Main (pureMain)
import qualified AOC.Parsija as P
import Data.Foldable (foldl')
import Data.List (nub)
import qualified Data.Map as M

type Polymer = [Char]

type Rules = M.Map (Char, Char) Char

parseInput :: P.Parser Char (Polymer, Rules)
parseInput =
  (,)
    <$> P.many1 P.upper <* P.spaces
    <*> (foldl' rule M.empty <$> P.many1 parseRule)
  where
    rule acc (a, b, c) = M.insert (a, b) c acc
    parseRule =
      (,,)
        <$> P.upper
        <*> P.upper <* P.string " -> " <* P.spaces
        <*> P.upper <* P.spaces

newtype Freqs a = Freqs {unFreqs :: M.Map a Int}

instance Ord a => Semigroup (Freqs a) where
  Freqs a <> Freqs b = Freqs $ M.unionWith (+) a b

instance Ord a => Monoid (Freqs a) where
  mempty = Freqs M.empty

singleton :: Ord a => a -> Freqs a
singleton x = Freqs $ M.singleton x 1

steps :: Rules -> Polymer -> Int -> Freqs Char
steps rules polymer0 n0 =
  foldMap singleton polymer0
    <> mconcat [between M.! (x, y, n0) | (x, y) <- zip polymer0 (drop 1 polymer0)]
  where
    elements =
      nub $
        polymer0
          <> [c | ((x, y), z) <- M.toList rules, c <- [x, y, z]]
    between = M.fromList $ do
      x <- elements
      y <- elements
      n <- [1 .. n0]
      let val = case M.lookup (x, y) rules of
            Nothing | n == 1 -> mempty
            Nothing -> between M.! (x, y, n - 1)
            Just z | n == 1 -> singleton z
            Just z -> singleton z <> between M.! (x, z, n - 1) <> between M.! (z, y, n - 1)
      pure ((x, y, n), val)

main :: IO ()
main = pureMain $ \input -> do
  (polymer0, rules) <- P.runParser parseInput input
  let freqs10 = unFreqs $ steps rules polymer0 10
      part1 = maximum freqs10 - minimum freqs10
      freqs40 = unFreqs $ steps rules polymer0 40
      part2 = maximum freqs40 - minimum freqs40

  pure (pure part1, pure part2)