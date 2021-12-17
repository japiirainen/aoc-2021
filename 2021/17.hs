module Main where

import Control.Monad (guard)

xmin, xmax, ymin, ymax :: Int
(xmin, xmax) = (281, 311)
(ymin, ymax) = (-74, -54)

arc :: (Int, Int) -> [(Int, Int)]
arc = takeWhile (\(_, y) -> y >= ymin) . steps (0, 0)
  where
    steps (x, y) (dx, dy) =
      (x, y) : steps (x + dx, y + dy) (dx - signum dx, dy - 1)

validArc :: [(Int, Int)] -> Bool
validArc = any (\(x, y) -> x >= xmin && x <= xmax && y >= ymin && y <= ymax)

as :: [[(Int, Int)]]
as = do
  dx <- [1 .. xmax]
  dy <- [ymin .. negate ymin]
  let x = arc (dx, dy)
  guard $ validArc x
  pure x

main :: IO ()
main = print (maximum . (concatMap . map) snd $ as) >> print (length as)
