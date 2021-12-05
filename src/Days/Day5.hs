module Days.Day5 where

import           Control.Applicative ((<|>))
import           Control.Arrow       ((&&&))

import qualified AOC.Parsija         as P
import qualified Data.List           as L
import qualified Data.Maybe          as M

type Point = (Int, Int)
type Line = (Point, Point)

parseInput :: P.Parser Char [Line]
parseInput = P.many1 $ (,) <$> (parseCoordinates <* parseArrow) <*> parseCoordinates <* (P.newline <|> P.spaces)
  where
    parseCoordinates = (,) <$> P.decimal <* P.char ',' <*> P.decimal
    parseArrow = P.spaces *> P.string "->" <* P.spaces


toHisto :: Ord a => [a] -> [(a, Int)]
toHisto = L.map (\xs@(x:_) -> (x, L.length xs)) . L.group . L.sort

ranges :: Line -> Maybe [Point]
ranges ((x1, y1), (x2, y2))
    | dx == 0 = Just $ [(x1, y1 + signY * d) | d <- [0..dy]]
    | dy == 0 = Just $ [(x1 + signX * d, y1) | d <- [0..dx]]
    | dx == dy = Just $ [(x1 + signX * d, y1 + signY * d) | d <- [0..dx]]
    | otherwise = Nothing
  where
      (dx, signX) = (abs &&& signum) (x2 - x1)
      (dy, signY) = (abs &&& signum) (y2 - y1)

isStraight :: Line -> Bool
isStraight ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

overlaps :: Ord a => [(a, Int)] -> Int
overlaps = L.length . L.filter ((>= 2) . snd) . toHisto

main :: IO ()
main = do
    inputStr <- readFile "inputs/day5.txt"
    case P.runParser parseInput inputStr of
        Left err -> putStrLn err
        Right input -> do
            let part1 = overlaps . L.concat . M.mapMaybe ranges . L.filter isStraight
                part2 = overlaps . L.concat . M.mapMaybe ranges
            print $ "Part 1: " ++ show (part1 input)
            print $ "Part 2: " ++ show (part2 input)
