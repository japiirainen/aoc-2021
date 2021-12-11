{-# LANGUAGE LambdaCase #-}

module Main where

import qualified AOC.Grid as G
import AOC.Main (pureMain)
import Data.Char (digitToInt, isDigit)
import Data.Traversable (for)

main :: IO ()
main = pureMain $ \input -> do
  grid <- for (G.fromString input) $ \case
    c | isDigit c -> Right $ digitToInt c
    c -> Left $ "Unknown char " ++ show c
  pure (pure (), pure ())
