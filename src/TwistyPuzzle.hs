{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}

-- |
-- Module      :  Twisty Puzzle
-- Copyright   :  (c) Grant Goodman 2021
-- Description :  A typeclass for Twisty Puzzles
-- License     :  MIT
-- Maintainer  :  zornslemonade@gmail.com
-- Portability :  Experimental
--
-- A module for working with twisty puzzles
module TwistyPuzzle (TwistyPuzzle (..)) where

import NumericPrelude
import Algebra.Ring as Ring
import Algebra.Additive as Additive
import Algebra.ToInteger as ToInteger
import Data.Semigroup
import Data.Monoid
import Data.Group
import qualified Algebra.IntegralDomain as IntegralDomain
import qualified Algebra.ZeroTestable as ZeroTestable

class (Group a) => TwistyPuzzle a where
  -- | Composition of puzzle configurations
  infixl 7 |#|

  (|#|) :: a -> a -> a
  (|#|) = (<>)

  -- | Exponentiation (including negative exponents)
  infixl 8 |#|^

  (|#|^) :: (Eq b, IntegralDomain.C b, ZeroTestable.C b) => a -> b -> a
  x |#|^ 0 = mempty
  x |#|^ (-1) = invert x
  x |#|^ n
    | even n = (x |#| x) |#|^ div n 2
    | otherwise = x |#| (x |#| x) |#|^ div n 2

  -- | Conjugation of puzzle configurations, i.e.,
  --
  -- > g |#|^|#| h == h |#|^ (-1) |#| g |#| h
  infix 8 |#|^|#|

  (|#|^|#|) :: a -> a -> a
  x |#|^|#| y = invert y |#| x |#| y

  -- | Commutator of puzzle configurations, i.e.,
  --
  -- > g >|#|< h == g |#|^ (-1) |#| h |#|^ (-1) |#| g |#| h
  infix 7 >|#|<

  (>|#|<) :: a -> a -> a
  x >|#|< y = invert x |#| invert y |#| x |#| y