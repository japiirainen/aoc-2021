{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import AOC.Coord (Coord (..), below, coordLines, right)
import AOC.Main (simpleMain)
import Data.Map (Map)
import Data.Map qualified as M

step1 :: Int -> Int -> Char -> (Coord -> Coord) -> Map Coord Char -> Map Coord Char
step1 ny nx c f inp = M.fromList $ do
  (k, v) <- M.toList inp
  let k' = fixup (f k)
  pure (if v == c && M.notMember k' inp then k' else k, v)
  where
    fixup :: Coord -> Coord
    fixup (C y x) = C (y `mod` ny) (x `mod` nx)

step :: Int -> Int -> Map Coord Char -> Map Coord Char
step ny nx = step1 ny nx 'v' below . step1 ny nx '>' right

same :: Eq a => [a] -> Int
same = go 1
  where
    go i (x : y : _) | x == y = i
    go i (_ : xs) = go (i + 1) xs
    go _ [] = error "no repeats :("

main :: IO ()
main = simpleMain $ \input ->
  let ipm = M.fromList $ coordLines (lines input)
      C ny nx = 1 + maximum (M.keys ipm)
      inp = M.filter (`elem` ">v") ipm
      steps = iterate (step ny nx) inp
   in (same steps, ())
