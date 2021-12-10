{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Applicative

import           AOC.Main            (pureMain)
import qualified AOC.Parsija         as P
import           Data.Foldable       (foldl')
import           Data.List           (sort)
import qualified Data.Map            as M

data Tok = OBrace | CBrace | OBracket | CBracket | OParen | CParen | OTag | CTag
    deriving (Eq, Show, Ord)

corruptedPoints :: Tok -> Int
corruptedPoints = \case
  CParen   -> 3
  CBracket -> 57
  CBrace   -> 1197
  CTag     -> 25137
  _        -> 0

incompletePoints :: [Tok] -> Int
incompletePoints = foldl' go 0
    where go acc x = acc * 5 + p x
            where p = \case
                    CParen   -> 1
                    CBracket -> 2
                    CBrace   -> 3
                    CTag     -> 4
                    _        -> 0

tokPairs :: M.Map Tok Tok
tokPairs = M.fromList [ (OBrace, CBrace)
                      , (OBracket, CBracket)
                      , (OParen, CParen)
                      , (OTag, CTag)
                      ]

type Line = [Tok]

parseLine :: P.Parser Char Line
parseLine = P.many1 $
         (OBrace <$ P.char '{')
     <|> (CBrace <$ P.char '}')
     <|> (OBracket <$ P.char '[')
     <|> (CBracket <$ P.char ']')
     <|> (OParen <$ P.char '(')
     <|> (CParen <$ P.char ')')
     <|> (OTag <$ P.char '<')
     <|> (CTag <$ P.char '>')

parseInput :: P.Parser Char [Line]
parseInput = parseLine `P.sepBy1` P.newline

data Validation
    = Ok
    | Corrupted Tok
    | Incomplete [Tok]

type Stack = [Tok]

validateLine :: Line -> Validation
validateLine = go []
    where
        go :: Stack -> Line -> Validation
        go [] [] = Ok
        go stack [] = Incomplete stack
        go stack (tok : toks)
            | Just tok' <- M.lookup tok tokPairs = go (tok' : stack) toks
            | s : stack' <- stack, s == tok = go stack' toks
            | otherwise = Corrupted tok

main :: IO ()
main = pureMain $ \input -> do
    ls <- P.runParser parseInput input
    let validatedLines = map validateLine ls
        part1 = sum [corruptedPoints tok | Corrupted tok <- validatedLines]
        part2 = let scores = [incompletePoints toks | Incomplete toks <- validatedLines]
                in sort scores !! (length scores `div` 2)
    pure (pure part1, pure part2)
