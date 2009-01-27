-- |
-- Module      : Control.Monad.Stream
-- Copyright   : Oleg Kiselyov
-- License     : PublicDomain
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

-- |
-- Non-Deterministic computations of type @Stream a@ can be enumerated
-- efficiently.
-- 
data Stream a = Nil | Single a | Cons a (Stream a) | Susp (Stream a)

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

instance MonadPlus Stream
 where
  mzero = Nil

  Nil       `mplus` ys = suspended ys             -- suspending
  Single x  `mplus` ys = Cons x ys
  Cons x xs `mplus` ys = Cons x (ys `mplus` xs)   -- interleaving
  Susp xs   `mplus` ys = ys `splus` xs            -- using `splus`

-- The function `splus` is similar to `mplus` but suspends its result
-- if the first argument is a suspension. It uses `mplus` in recursive
-- calls.
--
splus :: Stream a -> Stream a -> Stream a
Nil       `splus` ys = suspended ys               -- suspending
Single x  `splus` ys = Cons x ys
Cons x xs `splus` ys = Cons x (ys `mplus` xs)     -- interleaving
Susp xs   `splus` ys = suspended (ys `mplus` xs)  -- suspending, using `mplus`

