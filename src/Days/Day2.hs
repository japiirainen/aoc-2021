{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Days.Day2 where
import           Data.Foldable (foldl')

data Direction
    = Forward
    | Down
    | Up
    deriving (Show)

data Command = MkCommand Direction Int
    deriving (Show)

parseCommand :: String -> Command
parseCommand s = case words s of
    ["forward", n] -> MkCommand Forward (read n)
    ["down", n]    -> MkCommand Down (read n)
    ["up", n]      -> MkCommand Up (read n)
    _              -> error "Invalid input"

part1 :: [Command] -> Int
part1 cs = let (hor, dep) = foldl' step (0, 0) cs in hor * dep
    where
        step = \(hor, dep) -> \case
                MkCommand Forward n -> (hor + n, dep)
                MkCommand Down n    -> (hor, dep + n)
                MkCommand Up n      -> (hor, dep - n)

part2 :: [Command] -> Int
part2 cs = let (hor, dep, _) = foldl' step (0, 0, 0) cs in hor * dep
    where
        step = \(hor, dep, aim) -> \case
                MkCommand Forward n -> (hor + n, dep + (aim * n), aim)
                MkCommand Down n    -> (hor, dep, aim + n)
                MkCommand Up n      -> (hor, dep, aim - n)

main :: IO ()
main = do
    commands <- map parseCommand . lines <$> readFile "inputs/day2.txt" :: IO [Command]
    print $ "Part1: " <> show (part1 commands)
    print $ "Part2: " <> show (part2 commands)
    pure ()
