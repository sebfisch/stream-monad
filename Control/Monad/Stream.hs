-- |
-- Module      : Control.Monad.Stream
-- Copyright   : Oleg Kiselyov, Sebastian Fischer
-- License     : BSD3
-- 
-- Maintainer  : Sebastian Fischer (sebf@informatik.uni-kiel.de)
-- Stability   : experimental
-- Portability : portable
-- 
-- This Haskell library provides an implementation of the MonadPlus
-- type class that enumerates results of a non-deterministic
-- computation by interleaving subcomputations in a way that has
-- usually much better memory performance than other strategies with
-- the same termination properties.
-- 
-- By using supensions in strategic positions, the user can ensure
-- that the search does not diverge if there are remaining
-- non-deterministic results.
-- 
-- More information is available on the authors website:
-- <http://okmij.org/ftp/Computation/monads.html#fair-bt-stream>
-- 
-- Warning: @Stream@ is only a monad when the results of @runStream@
-- are interpreted as a multiset, i.e., a valid transformation
-- according to the monad laws may change the order of the results.
-- 
module Control.Monad.Stream ( Stream, suspended, runStream ) where

import Control.Monad
import Control.Applicative
import Control.Monad.Logic
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Prelude hiding (foldr)

-- |
-- Results of non-deterministic computations of type @Stream a@ can be
-- enumerated efficiently.
-- 
data Stream a = Nil | Single a | Cons a (Stream a) | Susp (Stream a)

instance Functor Stream
 where
  fmap _ Nil         = Nil
  fmap f (Single x)  = Single (f x)
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  fmap f (Susp xs)   = Susp (fmap f xs)

-- |
-- Suspensions can be used to ensure fairness.
-- 
suspended :: Stream a -> Stream a
suspended = Susp

-- |
-- The function @runStream@ enumerates the results of a
-- non-deterministic computation.
-- 
runStream :: Stream a -> [a]
runStream Nil         = []
runStream (Single x)  = [x]
runStream (Cons x xs) = x : runStream xs
runStream (Susp xs)   = runStream xs

instance Monad Stream
 where
  return = Single

  Nil       >>= _ = Nil
  Single x  >>= f = f x
  Cons x xs >>= f = f x `mplus` suspended (xs >>= f)
  Susp xs   >>= f = suspended (xs >>= f)

  fail _ = Nil

instance MonadPlus Stream
 where
  mzero = Nil

  Nil       `mplus` ys        = suspended ys                -- suspending
  Single x  `mplus` ys        = Cons x ys
  Cons x xs `mplus` ys        = Cons x (ys `mplus` xs)      -- interleaving
  xs        `mplus` Nil       = xs
  Susp xs   `mplus` Single y  = Cons y xs
  Susp xs   `mplus` Cons y ys = Cons y (xs `mplus` ys)
  Susp xs   `mplus` Susp ys   = suspended (xs `mplus` ys)

instance Applicative Stream where
  pure  = Single

  Nil       <*> _  = Nil
  Single f  <*> xs = fmap f xs
  Cons f fs <*> xs = fmap f xs <|> (xs <**> fs)
  Susp fs   <*> xs = suspended (xs <**> fs)

instance Alternative Stream where
  empty = Nil
  (<|>) = mplus

instance MonadLogic Stream where
   (>>-) = (>>=)
   interleave = mplus

   -- |
   -- The function @msplit@ splits a non-empty stream into a head and tail.
   --
   msplit Nil = return Nothing
   msplit (Single a) = return $ Just (a, Nil)
   msplit (Cons a r) = return $ Just (a, r)
   msplit (Susp i) = msplit i

   -- | 
   -- The function @ifte t th el@ is the logical conditional operator,
   -- equivalent to Prolog's "soft-cut". First the computation @t@ is
   -- executed. If it succeeds with at least one result, the entire
   -- @ifte@ computation is equivalent to @t >>= th@. Otherwise, the
   -- entire computation becomes equivalent to @el@.
   --
   ifte t th el = do 
     s <- msplit t
     case s of
       Nothing -> el
       Just (a, r) -> th a `mplus` (r >>= th)

   -- | 
   -- The function @once@ selects one solution out of possibly
   -- many. It greatly improves efficiency as it can be used to avoid
   -- useless backtracking and therefore to dispose of data structures
   -- that hold information needed for backtracking (e.g., choice
   -- points).
   --
   once m = do
     s <- msplit m
     case s of
       Nothing -> mzero
       Just (a, _) -> return a

instance Foldable Stream where
  foldMap _ Nil = mempty
  foldMap f (Single a) = f a
  foldMap f (Cons a r) = f a `mappend` foldMap f r
  foldMap f (Susp i) = foldMap f i

instance Traversable Stream where
  traverse _ Nil = pure Nil
  traverse f (Single a) = Single <$> f a
  traverse f (Cons a r) = Cons <$> f a <*> traverse f r
  traverse f (Susp i) = Susp <$> traverse f i
