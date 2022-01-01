module Main where

import AOC (Parser, guard, newline, number, parseE)
import AOC.Main (pureMain)

type Bounds = ((Int, Int), (Int, Int))

format :: Parser Bounds
format = (,) <$ "target area: x=" <*> range <* ", y=" <*> range <* newline
  where
    range = (,) <$> number <* ".." <*> number

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
  bounds <- parseE format input
  pure (pure (maximum . (concatMap . map) snd $ arcs bounds), pure (length $ arcs bounds))