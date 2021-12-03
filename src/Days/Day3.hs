module Days.Day3 where

import           Control.Applicative
import           Data.Maybe          (fromMaybe)

import qualified AOC.Parsija         as P
import qualified Data.Vector         as V

type Binary = V.Vector Bool

binaryToInt :: Binary -> Int
binaryToInt = V.foldl' (\a b -> a * 2 + if b then 1 else 0) 0

parseBinary :: P.Parser Char Binary
parseBinary = fmap V.fromList $ P.many1 $ (False <$ P.char '0') <|> (True <$ P.char '1')

parseBinaries :: P.Parser Char (V.Vector Binary)
parseBinaries = fmap V.fromList . many $ parseBinary <* P.spaces

mostCommonBit :: Binary -> Maybe Bool
mostCommonBit bin =
    case compare (V.length (V.filter id bin) * 2) (V.length bin) of
        EQ -> Nothing
        GT -> Just True
        LT -> Just False

powerConsumption :: V.Vector Binary -> Int
powerConsumption binary = binaryToInt gammaRate * binaryToInt epsilonRate
    where
        gammaRate = V.map (fromMaybe False . mostCommonBit) $ do
                        i <- V.enumFromN 0 (V.length (binary V.! 0))
                        pure $ (V.! i) <$> binary
        epsilonRate = not <$> gammaRate

withCriteria :: (Maybe Bool -> Bool -> Bool) -> V.Vector Binary -> Maybe Binary
withCriteria criteria = go 0
    where go i binary
            | V.length binary == 1 = Just (binary V.! 0)
            | otherwise = let mcb = mostCommonBit $ (V.! i) <$> binary
                            in go (i + 1) $ V.filter (\bin -> criteria mcb $ bin V.! i) binary

lifeSupportRating :: V.Vector Binary -> Int
lifeSupportRating binary = oxygenRating * co2Rating
    where
        oxygenRating = maybe 0 binaryToInt $
            withCriteria (\mcb b -> b == fromMaybe True mcb) binary
        co2Rating = maybe 0 binaryToInt $
            withCriteria (\mcb b -> b /= fromMaybe True mcb) binary

main :: IO ()
main = do
    fc <- readFile "inputs/day3.txt"
    case P.runParser parseBinaries fc of
        Left parseError -> print parseError
        Right binary    -> do
            print $ "Part 1: " <> show (powerConsumption binary)
            print $ "Part 2: " <> show (lifeSupportRating binary)

