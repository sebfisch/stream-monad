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
module Control.Monad.Stream ( Stream, suspended, runStream, toList ) where

import Control.Monad
import Control.Applicative
import Control.Monad.Logic
import Data.Foldable
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
runStream = toList
{-# DEPRECATED runStream "use toList" #-}

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
   (>>-)      = (>>=)
   interleave = mplus

   msplit Nil         = return Nothing
   msplit (Single x)  = return $ Just (x, Nil)
   msplit (Cons x xs) = return $ Just (x, suspended xs)
   msplit (Susp xs)   = suspended $ msplit xs

instance Foldable Stream where
  foldMap = foldMapDefault

instance Traversable Stream where
  traverse _ Nil         = pure Nil
  traverse f (Single x)  = Single <$> f x
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs
  traverse f (Susp xs)   = Susp <$> traverse f xs
