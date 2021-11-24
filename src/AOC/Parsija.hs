{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE NamedFieldPuns             #-}

-- | Small parser combinator library. (Applicative style)
module AOC.Parsija where

import           Control.Applicative (Alternative (..), Applicative (liftA2),
                                      (<**>), (<|>))
import           Data.Function       (on)
import           Data.List           (intercalate, intersperse)
import           Data.Maybe          (catMaybes)
import           Data.Set            (Set)

import           Control.Selective   (Selective (..))
import           Data.Foldable       (asum)
import qualified Data.Set            as Set

data Pos = Pos
    {_line :: Int
    , _col :: Int
    }

instance Eq Pos where
    (==) :: Pos -> Pos -> Bool
    e1 == e2 = _line e1 == _line e2 && _col e1 == _col e2

instance Ord Pos where
    (<=) :: Pos -> Pos -> Bool
    e1 <= e2 = _line e1 <= _line e2 && _col e1 <= _col e2

instance Show Pos where
    show :: Pos -> String
    show Pos{..} = "(" ++ show _line ++ ", " ++ show _col ++ ")"

data Input = Input
    { str  :: String
    , _pos:: Pos
    }

data Error = Oops
    { expected :: Set Item
    , unexpect :: Maybe Item
    , msgs     :: [String]
    , errPos   :: Pos
    } deriving stock Show

instance Eq Error where
    (==) :: Error -> Error -> Bool
    (==) = (==) `on` errPos

instance Ord Error where
    (<=) :: Error -> Error -> Bool
    (<=) = (<=) `on` errPos

newtype Max a = Max { getMax :: a }
instance (Alternative f, Ord a) => Semigroup (Max (f a)) where
    Max m <> Max n = Max $ (max <$> m <*> n) <|> m <|> n

instance Semigroup Error where
    e1 <> e2
        | e1 == e2 = e1
        { expected = expected e1 <> expected e2, unexpect = getMax (Max (unexpect e1) <> Max (unexpect e2)), msgs = msgs e1 <> msgs e2 }
        | e1 > e2 = e1
        | e1 < e2 = e2

format :: String -> Error -> String
format input Oops{..} =
    intercalate "\n " (catMaybes ([preamble `joinTogether` unexpectLine, expectedLine] ++ map Just msgs ++ [problem, caret]))
    where
        preamble = Just $ show errPos ++ ":"
        unexpectLine = ("unexpected " ++) . show <$> unexpect
        expectedLine = fmap ("expected " ++) $ foldMap Just $ intersperse ", " $ map show $ Set.toList expected
        inputLines = lines input
        problem = Just $ "> " ++ if null inputLines then "" else inputLines !! (_line errPos - 1)
        caret = Just $ "  " ++ replicate (_col errPos - 1) ' '  ++ "^"
        joinTogether p q = liftA2 (\x y -> x ++ " " ++ y) p q <|> p <|> q

data Item
    = Raw String
    | Named String
    | EndOfInput
    deriving stock (Eq, Ord)

instance Show Item where
    show :: Item -> String
    show = \case
      Raw " "     -> "space"
      Raw "\n"    -> "newline"
      Raw "\t"    -> "tab"
      Raw raw     -> show (takeWhile (/= ' ') raw)
      Named named -> named
      EndOfInput  -> "end of input"

newtype Hints = Hints [Set Item] deriving newtype (Semigroup, Monoid)

data State a r = State
    { input :: Input
    , good  :: a -> Input -> Hints -> Either String r
    , bad   :: Error -> Input -> Either String r
    }

newtype Parser a = Parser (forall r. State a r -> Either String r)

parse :: Parser a -> String -> Either String a
parse (Parser p) input = p $ State
    { input = Input {str = input, _pos = Pos{ _line = 1, _col = 1 }}
    , good = \x _ _ -> Right x
    , bad = \e _ -> Left (format input e)
    }

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser p) = Parser $ \st@State{..} -> p (st {
        good = good . f
    })

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser $ \State{..} -> good x input mempty

    liftA2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
    liftA2 f (Parser p) (Parser q) = Parser $ \st@State{..} ->
     let doQ x input hs = q (State {
         good = \y input' hs' -> good (f x y) input' (combineHints hs hs' (_pos input) (_pos input')),
         bad = \err input' -> bad (withHints err hs (_pos input) (_pos input')) input',
         input = input
       })
     in p (st {good = doQ})


instance Selective Parser where
    select :: Parser (Either a b) -> Parser (a -> b) -> Parser b
    select (Parser p) (Parser q) = Parser $ \st@State{..} ->
        let handle (Left x) = \input hs -> q (State {
            good = \f input' hs' -> good (f x) input' (combineHints hs hs' (_pos input) (_pos input')),
            bad = \err input' -> bad (withHints err hs (_pos input) (_pos input')) input',
            input = input
          })
            handle (Right x) = good x
        in p (st {good = handle})


instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ \State{..} ->
        bad (Oops {
            expected = mempty,
            unexpect = Nothing,
            msgs = [],
            errPos = _pos input
        }) input

    (<|>) :: Parser a -> Parser a -> Parser a
    Parser p <|> Parser q = Parser $ \st@State{..} ->
        let doQ err input'
                | _pos input < _pos input' = bad err input'
                | _pos input == _pos input' = q (st {
                    good = \x input' hs ->
                        if _pos input == _pos input' then good x input' (toHints err (_pos input') <> hs)
                        else good x input' hs,
                    bad = \err' -> bad (err <> err')
                })
        in p (st {bad = doQ})

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \State{..} -> case str input of
  c:cs | f c -> let p@Pos{..} = _pos input in case c of
      '\n' -> good '\n' (input {str = cs, _pos = p {_col = 1, _line = _line + 1}}) mempty
      c    -> good c (input {str = cs, _pos = p {_col = _col + 1}}) mempty
  cs -> bad (Oops {
      expected = Set.empty,
      unexpect = Just (foldr (const . Raw . pure) EndOfInput cs),
      msgs = [],
      errPos = _pos input
  }) input

try :: Parser a -> Parser a
try (Parser p) = Parser $ \st@State{..} -> p (st {bad = \err _ -> bad err input})

lookAhead :: Parser a -> Parser a
lookAhead (Parser p) = Parser $ \st@State{..} -> p (st {good = \x _ _ -> good x input mempty})

notFollowedBy :: Parser a -> Parser ()
notFollowedBy (Parser p) = Parser $ \st@State{..} ->
    let oldPos = _pos input
        item newPos = take (_col newPos - _col oldPos) $ head (lines (str input))
        err input' = Oops {
            expected = Set.empty,
            unexpect = Just $ if null (str input) then EndOfInput else Raw (item (_pos input')),
            msgs = [],
            errPos = oldPos
        }
    in p (st {
        good = \_ input' _ -> bad (err input') input,
        bad = \_ _ -> good () input mempty
    })

unexpected :: String -> Parser a
unexpected msg = Parser $ \State{bad, input} -> bad (Oops {
    expected = Set.empty,
    unexpect = Just (Named msg),
    msgs = [],
    errPos = _pos input
}) input

fail :: String -> Parser a
fail msg = Parser $ \State{bad, input} -> bad (Oops {
    expected = Set.empty,
    unexpect = Nothing,
    msgs = [msg],
    errPos = _pos input
}) input

line :: Parser Int
line = Parser $ \State{good, input} -> good (_line (_pos input)) input mempty

col :: Parser Int
col = Parser $ \State{good, input} -> good (_col (_pos input)) input mempty

infix 0 <?>
(<?>) :: Parser a -> String -> Parser a
Parser p <?> label = Parser $ \st@State{..} ->
    let label'
            | null label = Nothing
            | otherwise = Just (Named label)
        hintFix x input' hs
            | _pos input < _pos input', Nothing <- label' = good x input' (refreshLastHints hs Nothing)
            | _pos input < _pos input' = good x input' hs
            | _pos input == _pos input' = good x input (refreshLastHints hs label')
        labelApply err input' = flip bad input' $
            if _pos input == _pos input' then err { expected = maybe Set.empty Set.singleton label' }
            else err
    in p (st {good = hintFix, bad = labelApply})

combineHints :: Hints -> Hints -> Pos -> Pos -> Hints
combineHints hs hs' pos pos'
    | pos == pos' = hs <> hs'
    | pos < pos' = hs'
    | otherwise = error (show pos ++ " is not <= " ++ show pos')

withHints :: Error -> Hints -> Pos -> Pos -> Error
withHints err (Hints hs) pos pos'
    | pos' == pos = err { expected = Set.unions (expected err : hs) }

-- Taken from megaparsec
refreshLastHints :: Hints -> Maybe Item -> Hints
refreshLastHints (Hints []) _              = Hints []
refreshLastHints (Hints (_ : hs)) Nothing  = Hints hs
refreshLastHints (Hints (_ : hs)) (Just h) = Hints (Set.singleton h : hs)

-- Taken from megaparsec
toHints :: Error -> Pos -> Hints
toHints Oops{..} pos
    | errPos == pos = Hints [expected | not (Set.null expected)]
    | otherwise = mempty

------------------------------------------------------------------
-- COMBINATORS
------------------------------------------------------------------

infixr 4 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)

infixl 4 <~>
(<~>) :: Applicative f => f a -> f b -> f (a, b)
(<~>) = liftA2 (,)

choice :: Alternative f => [f a] -> f a
choice = asum

infixl 4 >?>
(>?>) :: (Selective f, Alternative f) => f a -> (a -> Bool) -> f a
fx >?> p = select ((\x -> if p x then Right x else Left ()) <$> fx) empty

filteredBy :: (Selective f, Alternative f) => f a -> (a -> Bool) -> f a
filteredBy = (>?>)

char :: Char -> Parser Char
char c = satisfy (== c) <?> show [c]

string :: String -> Parser String
string str = traverse char str <?> show str

item :: Parser Char
item = satisfy (const True) <?> "any character"

oneOf :: [Char] -> Parser Char
oneOf = choice . map char

noneOf :: [Char] -> Parser Char
noneOf = satisfy . (not .) . flip elem

eof :: Parser ()
eof = notFollowedBy item <?> "end of file"

pos :: Parser (Int, Int)
pos = line <~> col

infixl1 :: (a -> b) -> Parser a -> Parser (b -> a -> b) -> Parser b
infixl1 wrap p op = postfix wrap p (flip <$> op <*> p)

infixr1 :: (a -> b) -> Parser a -> Parser (a -> b -> b) -> Parser b
infixr1 wrap p op = p <**> (flip <$> op <*> infixr1 wrap p op <|> pure wrap)

prefix :: (a -> b) -> Parser (b -> b) -> Parser a -> Parser b
prefix wrap op p = op <*> prefix wrap op p <|> wrap <$> p

postfix :: (a -> b) -> Parser a -> Parser (b -> b) -> Parser b
postfix wrap p op = (wrap <$> p) <**> rest
    where
        rest = flip (.) <$> op <*> rest
            <|> pure id

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 = infixl1 id

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 = infixr1 id
