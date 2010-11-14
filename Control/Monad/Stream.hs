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
module Control.Monad.Stream ( Stream, suspended, runStream, 
                              runL, msplit, ifte, once, cut ) where

import Control.Monad
import Control.Applicative
import Prelude hiding (head, tail)

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

-- |
-- The function @msplit@ splits a non-empty stream into a head and tail.
--
msplit :: Stream a -> Maybe (a, Stream a)
msplit Nil = Nothing
msplit (Single a) = Just (a, Nil)
msplit (Cons a r) = Just (a, r)
msplit (Susp i) = msplit i

-- |
-- The function @runL@ enumerates the results of a
-- non-deterministic computation, limited by an optional count.
-- 
runL :: Maybe Int -> Stream a -> [a]
runL Nothing m = case msplit m of
  Nothing -> []
  Just (a, r) -> a:runL Nothing r
runL (Just i) _ | i == 0 = []
runL (Just i) m = case msplit m of
  Nothing -> []
  Just (a, r) -> a:runL (Just (i-1)) r

-- | 
-- The function @ifte t th el@ is the logical conditional operator,
-- equivalent to Prolog's "soft-cut". First the computation @t@ is
-- executed. If it succeeds with at least one result, the entire
-- @ifte@ computation is equivalent to @t >>= th@. Otherwise, the
-- entire computation becomes equivalent to @el@.
--
ifte :: Stream a -> (a -> Stream b) -> Stream b -> Stream b
ifte t th el = case msplit t of 
  Nothing -> el
  Just (a, r) -> th a `mplus` (r >>= th)

-- | 
-- The function @once@ selects one solution out of possibly
-- many. It greatly improves efficiency as it can be used to avoid
-- useless backtracking and therefore to dispose of data structures
-- that hold information needed for backtracking (e.g., choice
-- points).
--
once :: Stream a -> Stream a
once m = case msplit m of
  Nothing -> mzero
  Just (a, _) -> return a

-- |
-- The function @cut@ implements “negation as failure” and is
-- equivalent to "cut" in Prolog.
--
cut :: Stream a -> Stream ()
cut m = ifte (once m) (const mzero) (return ())

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
