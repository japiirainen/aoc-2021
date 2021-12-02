{-# LANGUAGE LambdaCase #-}
module Days.Day2 where

import           Control.Applicative
import           Data.Foldable       (foldl')

import qualified AOC.Parsija         as P

data Command = Forward !Int | Down !Int | Up !Int
    deriving (Show)

parseCommands :: P.Parser Char [Command]
parseCommands = many $
    (P.string "forward" *> P.spaces *> (Forward <$> P.decimal <* P.spaces)) <|>
    (P.string "down"    *> P.spaces *> (Down    <$> P.decimal <* P.spaces)) <|>
    (P.string "up"      *> P.spaces *> (Up      <$> P.decimal <* P.spaces))

part1 :: [Command] -> Int
part1 cs = let (hor, dep) = foldl' step (0, 0) cs in hor * dep
    where
        step = \(hor, dep) -> \case
                Forward n -> (hor + n, dep)
                Down n    -> (hor, dep + n)
                Up n      -> (hor, dep - n)

part2 :: [Command] -> Int
part2 cs = let (hor, dep, _) = foldl' step (0, 0, 0) cs in hor * dep
    where
        step = \(hor, dep, aim) -> \case
                Forward n -> (hor + n, dep + (aim * n), aim)
                Down n    -> (hor, dep, aim + n)
                Up n      -> (hor, dep, aim - n)

main :: IO ()
main = readFile "inputs/day2.txt" >>= \input -> do
    case P.runParser parseCommands input of
        Left e -> print e
        Right commands -> do
            print $ "Part 1: " <> show (part1 commands)
            print $ "Part 2: " <> show (part2 commands)
