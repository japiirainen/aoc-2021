module Main where

import           AOC.Main    (pureMain)

import qualified AOC.Parsija as P

parseInput :: P.Parser Char Int
parseInput = undefined

main :: IO ()
main = pureMain $ \input -> do
    ip <- P.runParser parseInput input

    pure (pure (), pure ())
