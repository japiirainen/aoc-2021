module Days.Day4 where

import           AOC.V2      (V2 (..))
import           Data.List   (partition)

import qualified AOC.Grid    as G
import qualified AOC.Parsija as P
import qualified Data.Map    as M
import qualified Data.Set    as S

parseInput :: P.Parser Char ([Int], [G.Grid Int])
parseInput = (,)
    <$> (P.sepBy1 P.decimal (P.char ',') <* eol <* eol)
    <*> P.sepBy1 parseBoard eol
  where
      eol = P.horizontalSpaces <* P.newline
      parseBoard = G.fromList <$> P.sepBy1 parseBoardLine eol <* eol
      parseBoardLine = P.horizontalSpaces *> P.sepBy1 P.decimal P.horizontalSpaces

data Line = Row !Int | Col !Int deriving (Eq, Ord)

wins :: Ord a => S.Set a -> G.Grid a -> Bool
wins drawn = or . M.foldrWithKey (\(V2 x y) val acc ->
    let ok = val `S.member` drawn in
        M.insertWith (&&) (Row y) ok $ M.insertWith (&&) (Col x) ok acc) M.empty

score :: (Num a, Ord a) => S.Set a -> G.Grid a -> a
score drawn = sum . filter (not . (`S.member` drawn)) . map snd . M.toList

play :: [Int] -> [G.Grid Int] -> [Int]
play = go S.empty
    where
        go _ [] _ = []
        go drawn (n:numbers) boards =
            [score drawn' w * n | w <- winners] ++ go drawn' numbers boards'
          where
              drawn' = S.insert n drawn
              (winners, boards') = partition (wins drawn') boards

main :: IO ()
main = do
    inputStr <- readFile "inputs/day4.txt"
    case P.runParser parseInput inputStr of
        Left err -> print err
        Right (ns, boards) -> do
            let winners = play ns boards
            print $ "Part 1: " ++ show (head winners)
            print $ "Part 2: " ++ show (last winners)
































{-}
type InputNumbers = [Int]

type Board = [[Int]]

-- >>> P.runParser parseNums "1,2,3,4,5"
-- Right [1,2,3,4,5]

parseNums :: P.Parser Char InputNumbers
parseNums = P.sepBy1 P.decimal (P.char ',') <* P.spaces

-- >>> sum $ filter (/= -1) $ concat [[1,-1,3],[-1,5,6]]
-- 15

sampleNums :: [Int]
sampleNums = [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]
sampleBoards :: [Board]
sampleBoards = [
                [[22, 13, 17, 11, 0],
                [8, 2, 23, 4, 24],
                [21, 9, 14, 16, 7],
                [6, 10, 3, 18, 5],
                [1, 12, 20, 15, 19]]
                ,
                [[3, 15, 0, 2, 22],
                [9, 18, 13, 17, 5],
                [19, 8, 7, 25, 23],
                [20, 11, 10, 24, 4],
                [14, 21, 16, 12, 6]],

                [[14, 21, 17, 24, 4],
                [10, 16, 15, 9, 19],
                [18, 8, 23, 26, 20],
                [22, 11, 13, 6, 5],
                [2, 0, 12, 3, 7]]
               ]

-- >>> part1 sampleNums sampleBoards
-- Just 0


part1 :: [Int] -> [Board] -> Maybe Int
part1 nums originalBoards =
    fst $ foldl' (\(mRes, bs) currNum ->
            let updatedBoards = map (updateBoard currNum) bs
                idx     = elemIndex True (map checkBoard updatedBoards)
            in if isJust idx
                then (Just $ sum $ filter (/= -1) $ concat (updatedBoards !! fromJust idx), updatedBoards)
                else (mRes, updatedBoards)
        ) (Nothing, originalBoards) nums


checkBoard :: Board -> Bool
checkBoard b =
    let vertical = map sum
        horizontal = map sum . transpose
        both = vertical b ++ horizontal b
    in elem (-5) both


updateBoard :: Int -> Board -> Board
updateBoard n b = map (map (updateTile n)) b
    where
        updateTile curr tile
            | curr == tile = -1
            | otherwise = tile

main :: IO ()
main = do
    nums <-  map read . lines <$> readFile "inputs/sample4.txt" :: IO [Int]
    print nums
    pure ()
-}
