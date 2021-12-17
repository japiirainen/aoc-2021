module Main where

import AOC.Main (pureMain)
import qualified AOC.Parsija as P
import Control.Monad (guard)

type Bounds = ((Int, Int), (Int, Int))

parseInput :: P.Parser Char Bounds
parseInput = (,) <$> px <*> py
  where
    px = (,) <$> (P.string "target area: x=" *> P.decimal) <*> (P.string ".." *> P.decimal <* P.string ",")
    py = (,) <$> (P.spaces *> P.string "y=-" *> (negate <$> P.decimal)) <*> (P.string "..-" *> (negate <$> P.decimal))

arc :: Bounds -> (Int, Int) -> [(Int, Int)]
arc (_, (ymin, _)) = takeWhile (\(_, y) -> y >= ymin) . steps (0, 0)
  where
    steps (x, y) (dx, dy) =
      (x, y) : steps (x + dx, y + dy) (dx - signum dx, dy - 1)

validArc :: Bounds -> [(Int, Int)] -> Bool
validArc ((xmin, xmax), (ymin, ymax)) =
  any (\(x, y) -> x >= xmin && x <= xmax && y >= ymin && y <= ymax)

arcs :: Bounds -> [[(Int, Int)]]
arcs bs@((_, xmax), (ymin, _)) = do
  dx <- [1 .. xmax]
  dy <- [ymin .. negate ymin]
  let x = arc bs (dx, dy)
  guard $ validArc bs x
  pure x

main :: IO ()
main = pureMain $ \input -> do
  bounds <- P.runParser parseInput input
  pure (pure (maximum . (concatMap . map) snd $ arcs bounds), pure (length $ arcs bounds))
