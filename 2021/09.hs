module Main where

import qualified AOC.Grid    as G
import           AOC.Main    (pureMain)

import qualified AOC.Parsija as P

parseInput :: P.Parser Char (G.Grid Int)
parseInput = G.fromList <$> parseBoardLine `P.sepBy1` P.newline
    where
        parseBoardLine = read <$> P.many1 P.char "" <* P.horizontalSpaces


main :: IO ()
main = do
    file <- readFile "2021/09-sample.txt"
    case P.runParser parseInput file of
        Left err -> print err
        Right grid -> do
            print grid
-- main :: IO ()
-- main = pureMain $ \input -> do
--     ip <- P.runParser parseInput input

--     pure (pure (), pure ())
