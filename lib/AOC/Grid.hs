module AOC.Grid where

import AOC.V2
import qualified AOC.V2.Box as Box
import Control.Monad (forM_)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified System.IO as IO

data Dir = U | R | D | L deriving (Bounded, Enum, Eq, Ord, Show)

turnRight, turnLeft, turnAround :: Dir -> Dir
turnRight d = if d == maxBound then minBound else succ d
turnLeft d = if d == minBound then maxBound else pred d
turnAround U = D
turnAround D = U
turnAround L = R
turnAround R = L

type Pos = V2 Int

origin :: Pos
origin = zero

neighbors :: Pos -> [Pos]
neighbors (V2 x y) = [V2 x (y - 1), V2 (x + 1) y, V2 x (y + 1), V2 (x - 1) y]

diagonal :: Pos -> [Pos]
diagonal (V2 x y) =
  [ V2 (x - 1) (y - 1),
    V2 (x - 1) (y + 1),
    V2 (x + 1) (y - 1),
    V2 (x + 1) (y + 1)
  ]

manhattan :: Pos -> Pos -> Int
manhattan (V2 lx ly) (V2 rx ry) = abs (lx - rx) + abs (ly - ry)

move :: Int -> Dir -> Pos -> Pos
move n dir (V2 x y) = case dir of
  U -> V2 x (y - n)
  L -> V2 (x - n) y
  D -> V2 x (y + n)
  R -> V2 (x + n) y

type Grid a = M.Map Pos a

-- >>> fromList [[1,2], [1,2]]
-- Just (Box {bTopLeft = V2 {vX = 0, vY = 0}, bBottomRight = V2 {vX = 1, vY = 1}})

fromList :: [[a]] -> Grid a
fromList =
  L.foldl'
    ( \acc (y, row) ->
        L.foldl'
          (\m (x, c) -> M.insert (V2 x y) c m)
          acc
          (zip [0 ..] row)
    )
    M.empty
    . zip [0 ..]

fromString :: String -> Grid Char
fromString = fromList . lines

toString :: Grid Char -> String
toString grid = case box grid of
  Nothing -> "<empty grid>"
  Just (Box.Box (V2 minX minY) (V2 maxX maxY)) -> unlines $ do
    y <- [minY .. maxY]
    pure [fromMaybe ' ' (M.lookup (V2 x y) grid) | x <- [minX .. maxX]]

readGrid :: (Char -> IO a) -> IO.Handle -> IO (Grid a)
readGrid f h = IO.hGetContents h >>= traverse f . fromString

printGrid :: IO.Handle -> Grid Char -> IO ()
printGrid h grid = case box grid of
  Nothing -> IO.hPutStrLn h "<empty grid>"
  Just (Box.Box (V2 minX minY) (V2 maxX maxY)) ->
    forM_ [minY .. maxY] $ \y ->
      IO.hPutStrLn
        h
        [fromMaybe ' ' (M.lookup (V2 x y) grid) | x <- [minX .. maxX]]

center :: Grid a -> Maybe Pos
center grid = case M.maxViewWithKey grid of
  Nothing -> Nothing
  Just ((V2 x y, _), _) -> Just $ V2 (x `div` 2) (y `div` 2)

box :: Grid a -> Maybe (Box.Box Int)
box grid
  | M.null grid = Nothing
  | otherwise = Just $ L.foldl1' (<>) $ map Box.fromV2 $ M.keys grid
