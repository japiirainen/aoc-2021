{-# LANGUAGE DerivingVia #-}
module Main where

import           AOC.Main            (pureMain)
import qualified AOC.Parsija         as P
import           Control.Applicative (many, (<|>))
import qualified Data.Map            as M
import           Data.Monoid         (Sum (..))

newtype Fish = MkFish Int
    deriving stock (Show, Eq, Ord)

newtype Population = Pop { unPop :: M.Map Fish Int } deriving (Show)
instance Semigroup Population where Pop l <> Pop r = Pop $ M.unionWith (+) l r
instance Monoid Population where mempty = Pop mempty

step :: Fish -> [Fish]
step (MkFish 0) = [MkFish 6, MkFish 8]
step (MkFish n) = [MkFish $ n - 1]

popSize :: Population -> Int
popSize = getSum . foldMap Sum . unPop

singleton :: Fish -> Population
singleton fish = Pop $ M.singleton fish 1

times :: Int -> Population -> Population
times n (Pop pop) = Pop ((* n) <$> pop)

stepPopulation :: Population -> Population
stepPopulation = M.foldMapWithKey
    (\fish freq -> times freq . foldMap singleton $ step fish) . unPop

main :: IO ()
main = pureMain $ \input -> do
  xs <- P.runParser parseFish input
  let r1 = length $ iterate (concatMap step) xs !! 80
      r2 = popSize $ iterate stepPopulation (foldMap singleton xs)!! 256
  pure (pure r1, pure r2)

parseFish :: P.Parser Char [Fish]
parseFish = many $ MkFish <$> P.decimal <* (P.char ',' <|> P.spaces)
