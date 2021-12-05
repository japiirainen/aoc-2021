{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | ListT monad transformer.
module AOC.List.Transformer where

import           Control.Applicative  (Alternative (..), Applicative (liftA2))
import           Control.Monad        (MonadPlus (..))
import           Control.Monad.Except (MonadError (..))
import           Control.Monad.Reader (MonadReader (..))
import           Control.Monad.State  (MonadState (..))
import           Control.Monad.Trans  (MonadIO (..), MonadTrans (..))
import           Prelude              hiding (drop, dropWhile, pred, take,
                                       takeWhile, zip)

import qualified Data.Foldable
import qualified Data.Semigroup

newtype ListT m a = ListT { next :: m (Step m a) }
    deriving (Foldable, Traversable)

instance MonadTrans ListT where
    lift m = ListT $ do
        x <- m
        return $ Cons x empty

instance Monad m => Functor (ListT m) where
    fmap f (ListT m) = ListT $ do
        fmap f <$> m

instance Monad m => Applicative (ListT m) where
    pure x = ListT $ return $ Cons x empty

    ListT m <*> l = ListT $ do
        s <- m
        case s of
            Nil       -> return Nil
            Cons f l' -> next (fmap f l <|> (l' <*> l))

    ListT m *> l = ListT $ do
        s <- m
        case s of
            Nil       -> return Nil
            Cons _ l' -> next (l <|> (l' *> l))

    ListT m <* l = ListT $ do
        s <- m
        case s of
            Nil       -> return Nil
            Cons x l' -> next ((x <$ l) <|> (l' <* l))

instance Monad m => Monad (ListT m) where
    return = pure

    ListT m >>= f = ListT $ do
        s <- m
        case s of
            Nil       -> return Nil
            Cons x l' -> next (f x <|> (l' >>= f))

instance Monad m => Alternative (ListT m) where
    empty = ListT $ return Nil

    ListT m <|> l = ListT $ do
        s <- m
        case s of
            Nil       -> next l
            Cons x l' -> return $ Cons x (l' <|> l)

instance Monad m => MonadPlus (ListT m) where
    mzero = empty
    mplus = (<|>)

instance Monad m => MonadFail (ListT m) where
    fail _ = mzero

instance (Monad m, Data.Semigroup.Semigroup a) => Data.Semigroup.Semigroup (ListT m a) where
    (<>) = liftA2 (<>)

instance (Monad m, Data.Semigroup.Semigroup a, Monoid a) => Monoid (ListT m a) where
    mempty = pure mempty

instance MonadIO m => MonadIO (ListT m) where
    liftIO m = lift (liftIO m)

instance MonadError e m => MonadError e (ListT m) where
    throwError e = ListT $ throwError e
    catchError (ListT m) k = ListT $  catchError m (next . k)

instance MonadReader i m => MonadReader i (ListT m) where
    ask = lift ask

    local k (ListT m) = ListT $ do
        s <- local k m
        case s of
            Nil      -> return Nil
            Cons x l -> return $ Cons x (local k l)

instance MonadState s m => MonadState s (ListT m) where
    get = lift get
    put x = lift (put x)
    state k = lift (state k)

instance (Monad m, Num a) => Num (ListT m a) where
    fromInteger n = pure $ fromInteger n

    negate = fmap negate
    abs = fmap abs
    signum = fmap signum

    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

instance (Monad m, Fractional a) => Fractional (ListT m a) where
    fromRational n = pure $ fromRational n
    recip = fmap recip
    (/) = liftA2 (/)

instance (Monad m, Floating a) => Floating (ListT m a) where
    pi = pure pi
    exp  = fmap exp
    sqrt = fmap sqrt
    log  = fmap log
    sin  = fmap sin
    tan  = fmap tan
    cos  = fmap cos
    asin = fmap asin
    atan = fmap atan
    acos = fmap acos
    sinh = fmap sinh
    tanh = fmap tanh
    cosh = fmap cosh
    asinh = fmap asinh
    atanh = fmap atanh
    acosh = fmap acosh
    (**)    = liftA2 (**)
    logBase = liftA2 logBase

runListT :: Monad m => ListT m a -> m ()
runListT (ListT m) = do
    s <- m
    case s of
        Nil      -> return ()
        Cons _ l -> runListT l

fold :: Monad m => (x -> a -> x) -> x -> (x -> b) -> ListT m a -> m b
fold step begin done= go begin
  where
    go !x (ListT m) = do
      s <- m
      case s of
        Nil      -> return (done x)
        Cons a l -> go (step x a) l

foldM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> ListT m a -> m b
foldM step begin done l0 = do
    x0 <- begin
    go x0 l0
  where
    go !x (ListT m) = do
      s <- m
      case s of
        Nil -> done x
        Cons a l' -> do
          x' <- step x a
          go x' l'

select :: (Foldable f, Alternative m) => f a -> m a
select = Data.Foldable.foldr cons empty
  where
    cons x xs = pure x <|> xs

take :: Monad m => Int -> ListT m a -> ListT m a
take n l
  | n <= 0    = empty
  | otherwise = ListT $ do
      s <- next l
      case s of
        Nil       -> return Nil
        Cons a l' -> return (Cons a (take (n-1) l'))

drop :: Monad m => Int -> ListT m a -> ListT m a
drop n l
  | n <= 0    = l
  | otherwise = ListT $ do
      s <- next l
      case s of
        Nil       -> return Nil
        Cons _ l' -> next (drop (n-1) l')

dropWhile :: Monad m => (a -> Bool) -> ListT m a -> ListT m a
dropWhile pred l = ListT $ do
  n <- next l
  case n of
    Cons x l'
      | pred x -> next (dropWhile pred l')
      | otherwise -> return (Cons x l')
    Nil -> return Nil

takeWhile :: Monad m => (a -> Bool) -> ListT m a -> ListT m a
takeWhile pred l = ListT $ do
  n <- next l
  case n of
    Cons x l' | pred x -> return (Cons x (takeWhile pred l'))
    _                  -> return Nil

unfold :: Monad m => (b -> m (Maybe (a, b))) -> b -> ListT m a
unfold step = loop
    where
      loop seed = ListT $ do
        mx <- step seed
        case mx of
          Just (x, seed') -> return (Cons x (loop seed'))
          Nothing         -> return Nil

zip :: Monad m => ListT m a -> ListT m b -> ListT m (a, b)
zip xs ys = ListT $ do
  sx <- next xs
  sy <- next ys
  case (sx, sy) of
    (Cons x xs', Cons y ys') -> return $ Cons (x, y) (zip xs' ys')
    _                        -> return Nil

data Step m a = Cons a (ListT m a) | Nil
    deriving (Foldable, Traversable)

instance Monad m => Functor (Step m) where
    fmap _ Nil        = Nil
    fmap f (Cons x l) = Cons (f x) (fmap f l)

newtype ZipListT m a = ZipListT { getZipListT :: ListT m a }
    deriving (Functor, Alternative, Foldable, Traversable, MonadTrans, Floating, Fractional, Num, Semigroup, Monoid)

instance Monad m => Applicative (ZipListT m) where
  pure x = ZipListT go
    where go = ListT (pure (Cons x go))

  ZipListT fs <*> ZipListT xs = ZipListT (fmap (uncurry ($)) (zip fs xs))
