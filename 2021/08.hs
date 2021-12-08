module Main where

import           AOC.Main            (pureMain)
import           Control.Applicative
import           Data.List
import           Data.Map            (Map)

import qualified AOC.Parsija         as P
import qualified Data.Map            as Map

type SigPat = [String]
type Output = [String]

parseInput :: P.Parser Char [(SigPat, Output)]
parseInput = many $ (,) <$> (sepBySpace <* P.char '|' <* P.spaces) <*> sepBySpace
  where sepBySpace = P.many1 P.alpha `P.sepBy1` P.char ' ' <* P.spaces


findMapping :: SigPat -> Map Char Char
findMapping xs = Map.fromList [(a, 'a'), (b, 'b'), (c, 'c'), (d, 'd'), (e, 'e'), (f, 'f'), (g, 'g')]
  where
    [one] = filter ((== 2) . length) xs
    [four] = filter ((== 4) . length) xs
    [seven] = filter ((== 3) . length) xs
    zeroSixNine = filter ((== 6) . length) xs
    twoThreeFive = filter ((== 5) . length) xs
    (zeroNine, [six]) = partition ((== 2) . length . intersect one) zeroSixNine
    ([nine], [zero]) = partition (elem d) zeroNine
    ([five], twoThree) = partition (null . (\\ six)) twoThreeFive
    ([two], [three]) = partition ((== 3) . length . intersect five) twoThree
    (a:_) = seven \\ one
    [b] = five \\ three
    [c] = nine \\ six
    [d] = foldl intersect four twoThreeFive
    [e] = zero \\ nine
    [f] = seven \\ two
    [g] = (nine \\ four) \\ seven

signalToNumber :: String -> Int
signalToNumber "abcefg"  = 0
signalToNumber "cf"      = 1
signalToNumber "acdeg"   = 2
signalToNumber "acdfg"   = 3
signalToNumber "bcdf"    = 4
signalToNumber "abdfg"   = 5
signalToNumber "abdefg"  = 6
signalToNumber "acf"     = 7
signalToNumber "abcdefg" = 8
signalToNumber "abcdfg"  = 9
signalToNumber x         = error $ "Unknown signal: " ++ x

decode :: Map Char Char -> String -> Int
decode mapping = signalToNumber . sort . map (mapping Map.!)

part1 :: [(SigPat, Output)] -> Int
part1 = length . filter isUnique . concatMap snd
  where isUnique x = length x `elem` [2, 3, 4, 7]

solve :: (SigPat, Output) -> Int
solve (sigPat, output) = let mapping = findMapping sigPat
  in read $ concatMap (show . decode mapping) output

main :: IO ()
main = pureMain $ \input -> do
  ip <- P.runParser parseInput input
  let p2 = sum . map solve
  pure (pure (part1 ip), pure (p2 ip))
