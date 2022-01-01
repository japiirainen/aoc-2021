{-# OPTIONS_GHC -Wno-missing-methods #-}

module Main where

import AOC
import AOC.Main (pureMain)

data SnailFish = Pure Int | Pair SnailFish SnailFish deriving Eq

parseSnailFish :: Parser SnailFish
parseSnailFish = Pure <$> decimal <|> pair
  where
    pair = Pair <$ char '[' <*> parseSnailFish <* char ',' <*> parseSnailFish <* char ']'

explode :: SnailFish -> Maybe SnailFish
explode s = mid <$> go (0 :: Int) s
  where
    go d (Pair (Pure a) (Pure b)) | d >= 4 = Just (rightMost (+ a), Pure 0, leftMost (+ b))
    go d (Pair a b) =
      (\(f, y, z) -> (f, Pair y (z b), id)) <$> go (d + 1) a
        <|> (\(f, y, z) -> (id, Pair (f a) y, z)) <$> go (d + 1) b
    go _ _ = Nothing

    leftMost f (Pure n) = Pure (f n)
    leftMost f (Pair a b) = Pair (leftMost f a) b
    rightMost f (Pure n) = Pure (f n)
    rightMost f (Pair a b) = Pair a (rightMost f b)
    mid (_, x, _) = x

magnitude :: SnailFish -> Int
magnitude (Pure n) = n
magnitude (Pair a b) = 3 * magnitude a + 2 * magnitude b

split :: SnailFish -> Maybe SnailFish
split (Pure n)
  | n >= 10 = Just $ Pair (Pure $ n `div` 2) (Pure $ (n + 1) `div` 2)
  | otherwise = Nothing
split (Pair a b) = flip Pair b <$> split a <|> Pair a <$> split b

reduce :: SnailFish -> Maybe SnailFish
reduce x = explode x <|> split x

final :: SnailFish -> SnailFish
final x = maybe x final $ reduce x

instance Num SnailFish where
  a + b = final (Pair a b)
  fromInteger = Pure . fromInteger

main :: IO ()
main = pureMain $ \input -> do
  ns <- parseE (eachLine parseSnailFish) input
  let p1 = magnitude $ sum ns
      p2 = maximum [magnitude (x + y) | x <- ns, y <- ns]
  pure (pure p1, pure p2)
