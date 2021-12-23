{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import AOC.Coord
  ( Coord (..),
    below,
    cardinal,
    coordCol,
    coordLines,
    coordRow,
    manhattan,
  )
import AOC.Main (simpleMain)
import AOC.Search (AStep (AStep), astar, dfs)
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as M

isHallway :: Coord -> Bool
isHallway c = coordRow c == 1 && not (isRoom c)

isRoom :: Coord -> Bool
isRoom (C _ c) = c == 3 || c == 5 || c == 7 || c == 9

step :: Map Coord Char -> [AStep (Map Coord Char)]
step w =
  [ AStep w' (manhattan c dest * tokCost tok) 0
    | (c, tok) <- M.toList w,
      tok `elem` "ABCD",
      dest <- route w c,
      if isRoom c
        then isHallway dest
        else
          isRoom dest
            && coordCol dest == roomCol tok
            && roomClean w tok
            && (w M.! below dest) `elem` [tok, '#'],
      let w' =
            M.insert c '.' $
              M.insert dest tok w
  ]

roomClean :: Map Coord Char -> Char -> Bool
roomClean w tok =
  all (`elem` [tok, '.']) $
    takeWhile ('#' /=) [w M.! C r (roomCol tok) | r <- [2 ..]]

done :: Map Coord Char -> Bool
done w =
  all @[]
    ( \tok ->
        all (tok ==) $
          takeWhile ('#' /=) [w M.! C r (roomCol tok) | r <- [2 ..]]
    )
    "ABCD"

roomCol :: Char -> Int
roomCol = \case
  'A' -> 3
  'B' -> 5
  'C' -> 7
  'D' -> 9
  _ -> error "invalid token"

tokCost :: Char -> Int
tokCost = \case
  'A' -> 1
  'B' -> 10
  'C' -> 100
  'D' -> 1000
  _ -> error "invalid token"

route :: Map Coord Char -> Coord -> [Coord]
route w = dfs move
  where
    move c = [c' | c' <- cardinal c, w M.! c' == '.']

main :: IO ()
main = simpleMain $ \input ->
  let inp = M.fromList (coordLines (lines input))
      Just (_, cost) = find (done . fst) (astar step inp)
   in (cost, ())