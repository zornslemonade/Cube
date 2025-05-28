{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

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

import qualified Algebra.IntegralDomain as IntegralDomain
import qualified Algebra.ZeroTestable as ZeroTestable
import qualified Data.Group as G
import Data.Monoid (Monoid (mempty, mappend), (<>))
import NumericPrelude
import qualified Algebra.RealIntegral
import qualified Permutation as P
import Action
import Data.Semigroup
import Data.Bits (Bits(xor))
import Number.DimensionTerm.SI (yard)

class (G.Group perm, G.Group orient, Action perm orient, Ord piece) => TwistyPuzzle config perm orient piece | config -> perm orient piece where
  getPermutations :: config -> perm

  getOrientations :: config -> orient

  constructConfig :: perm -> orient -> config

  -- | Composition of puzzle configurations
  infixl 7 |#|

  (|#|) :: config -> config -> config
  x |#| y = constructConfig (xp <> yp) (xo *? yp <> yo)
    where
      xp = getPermutations x
      xo = getOrientations x
      yp = getPermutations y
      yo = getOrientations y

  solved :: config
  solved = constructConfig mempty mempty

  invert :: config -> config
  invert x = constructConfig (G.invert xp) (xp ?* G.invert xo)
    where
      xp = getPermutations x
      xo = getOrientations x

  -- | Exponentiation (including negative exponents)
  infixl 8 |#|^

  (|#|^) :: (Eq n, IntegralDomain.C n, ZeroTestable.C n) => config -> n -> config
  x |#|^ 0 = solved
  x |#|^ (-1) = invert x
  x |#|^ n
    | even n = (x |#| x) |#|^ div n 2
    | otherwise = x |#| (x |#| x) |#|^ div n 2

  -- | Conjugation of puzzle configurations, i.e.,
  --
  -- > g |#|^|#| h == h |#|^ (-1) |#| g |#| h
  infix 8 |#|^|#|

  (|#|^|#|) :: config -> config -> config
  x |#|^|#| y = invert y |#| x |#| y

  -- | Commutator of puzzle configurations, i.e.,
  --
  -- > g >|#|< h == g |#|^ (-1) |#| h |#|^ (-1) |#| g |#| h
  infix 7 >|#|<

  (>|#|<) :: config -> config -> config
  x >|#|< y = invert x |#| invert y |#| x |#| y

  toPermutation :: config -> P.Permutation piece

  fromPermutation :: P.Permutation piece -> Maybe config

  order :: config -> Int
  order  = P.order . toPermutation

  {-# MINIMAL getPermutations, getOrientations, constructConfig, toPermutation, fromPermutation #-}