{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | ListT monad transformer.
module AOC.List.Transformer where

import           Control.Applicative  (Alternative (..), Applicative (liftA2))
import           Control.Monad        (MonadPlus (..))
import           Control.Monad.Except (MonadError (..))
import           Control.Monad.Reader (MonadReader (..))
import           Control.Monad.State  (MonadState (..))
import           Control.Monad.Trans  (MonadIO (..), MonadTrans (..))

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

data Step m a = Cons a (ListT m a) | Nil
    deriving (Foldable, Traversable)

instance Monad m => Functor (Step m) where
    fmap _ Nil        = Nil
    fmap f (Cons x l) = Cons (f x) (fmap f l)

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
