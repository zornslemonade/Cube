{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE InstanceSigs #-}
  
  -- |
-- Module      :  Permutable
-- Copyright   :  (c) Grant Goodman 2021
-- Description :  Implementation of the Generalized Symmetric Group
-- License     :  MIT
-- Maintainer  :  zornslemonade@gmail.com
-- Portability :  Experimental
--
-- A module which implements the generalized symmetric group for a few specific orders.
module Permutable (
  Permutable (..),
  -- * Tuple Newtypes
  Tuple6 (..),
  Tuple8 (..),
  Tuple12 (..),
  -- * Alternate Constructors
  t6,
  t8,
  t12
) where

import Data.Foldable
import qualified Data.List as L
import qualified Data.Map as M
import Permutation
import Test.Tasty.QuickCheck

import NumericPrelude
import Algebra.Ring as Ring
import Algebra.Additive as Additive
import Algebra.ToInteger as ToInteger
import Data.Semigroup
import Data.Monoid
import Data.Group

------
-- Defining an Permutable type class
------

-- This class is meant for container types which admit a well-defined action
-- by the symmetric group S_n, where n is their length.

class (Foldable z) => Permutable z where
  -- 1-based indexing
  infixl 9 *!
  (*!) :: (Ring.C b, Eq b) => z a -> b -> a

  -- Inverse of toList
  fromList :: [a] -> z a

  -- Utility function
  toPairList :: (Ring.C b, Ord b, Enum b) => z a -> [(b, a)]
  toPairList = zip [1 ..] . toList

  -- | Right action by permutations
  infixr 7 *?
  (*?) :: (Ring.C b, Ord b, Enum b) => z a -> Permutation b -> z a
  x *? o = invert o ?* x

  -- | Left action by permutations
  infixl 7 ?*
  (?*) :: (Ring.C b, Ord b, Enum b) => Permutation b -> z a -> z a
  o ?* x = fromList $ M.elems $ M.fromList [(o ?. n, y) | (n, y) <- toPairList x]

------
-- Defining type synonyms for ordered tuples which contain only one type
-- (Only the three needed for the CubeConfiguration type are defined)
------

-- Newtype for sextuples

newtype Tuple6 a = T6 (a, a, a, a, a, a) deriving (Eq)

t6 :: a -> a -> a -> a -> a -> a -> Tuple6 a
t6 = (((((T6 .) .) .) .) .) . (,,,,,)

instance Permutable Tuple6 where
  fromList [a, b, c, d, e, f] = T6 (a, b, c, d, e, f)
  fromList _ = error "Tuple6: Given list does not have exactly 6 elements"

  (T6 (a, b, c, d, e, f)) *! n =
    case n of
      1 -> a
      2 -> b
      3 -> c
      4 -> d
      5 -> e
      6 -> f
      _ -> error "Tuple6: Tuple index out of bounds"

instance Additive.C a => Additive.C (Tuple6 a) where
  zero = T6 (zero, zero, zero, zero, zero, zero)
  negate (T6 (a, b, c, d, e, f)) = T6 (negate a, negate b, negate c, negate d, negate e, negate f)
  (T6 (a, b, c, d, e, f)) + (T6 (a', b', c', d', e', f')) = T6 (a + a', b + b', c + c', d + d', e + e', f + f')

instance Ring.C a => Ring.C (Tuple6 a) where
  one = T6 (one, one, one, one, one, one)
  (*) :: Ring.C a => Tuple6 a -> Tuple6 a -> Tuple6 a
  (T6 (a, b, c, d, e, f)) * (T6 (a', b', c', d', e', f')) = T6 (a * a', b * b', c * c', d * d', e * e', f * f')

instance Foldable Tuple6 where
  foldMap mf (T6 (a, b, c, d, e, f)) = mf a `mappend` mf b `mappend` mf c `mappend` mf d `mappend` mf e `mappend` mf f

instance Show a => Show (Tuple6 a) where
  show (T6 x) = show x

-- Newtype for Octuples
newtype Tuple8 a = T8 (a, a, a, a, a, a, a, a) deriving (Eq)

t8 :: a -> a -> a -> a -> a -> a -> a -> a -> Tuple8 a
t8 = (((((((T8 .) .) .) .) .) .) .) . (,,,,,,,)

instance Permutable Tuple8 where
  fromList [a, b, c, d, e, f, g, h] = T8 (a, b, c, d, e, f, g, h)
  fromList _ = error "Tuple8: Given list does not have exactly 8 elements"

  (T8 (a, b, c, d, e, f, g, h)) *! n =
    case n of
      1 -> a
      2 -> b
      3 -> c
      4 -> d
      5 -> e
      6 -> f
      7 -> g
      8 -> h
      _ -> error "Tuple8: Tuple index out of bounds"

instance Additive.C a => Additive.C (Tuple8 a) where
  zero = T8 (zero, zero, zero, zero, zero, zero, zero, zero)
  negate (T8 (a, b, c, d, e, f, g, h)) = T8 (negate a, negate b, negate c, negate d, negate e, negate f, negate g, negate h)
  (T8 (a, b, c, d, e, f, g, h)) + (T8 (a', b', c', d', e', f', g', h')) = T8 (a + a', b + b', c + c', d + d', e + e', f + f', g + g', h + h')

instance Ring.C a => Ring.C (Tuple8 a) where
  one = T8 (one, one, one, one, one, one, one, one)
  (T8 (a, b, c, d, e, f, g, h)) * (T8 (a', b', c', d', e', f', g', h')) = T8 (a * a', b * b', c * c', d * d', e * e', f * f', g * g', h * h')

instance Foldable Tuple8 where
  foldMap mf (T8 (a, b, c, d, e, f, g, h)) = mf a `mappend` mf b `mappend` mf c `mappend` mf d `mappend` mf e `mappend` mf f `mappend` mf g `mappend` mf h

instance Show a => Show (Tuple8 a) where
  show (T8 x) = show x

-- Newtype for Duodecuples

newtype Tuple12 a = T12 (a, a, a, a, a, a, a, a, a, a, a, a) deriving (Eq)

t12 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Tuple12 a
t12 = (((((((((((T12 .) .) .) .) .) .) .) .) .) .) .) . (,,,,,,,,,,,)

instance Permutable Tuple12 where
  fromList [a, b, c, d, e, f, g, h, i, j, k, l] = T12 (a, b, c, d, e, f, g, h, i, j, k, l)
  fromList _ = error "Tuple12: Given list does not have exactly 12 elements"

  (T12 (a, b, c, d, e, f, g, h, i, j, k, l)) *! n =
    case n of
      1 -> a
      2 -> b
      3 -> c
      4 -> d
      5 -> e
      6 -> f
      7 -> g
      8 -> h
      9 -> i
      10 -> j
      11 -> k
      12 -> l
      _ -> error "Tuple12: Tuple index out of bounds"

instance Additive.C a => Additive.C (Tuple12 a) where
  zero = T12 (zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero)
  negate (T12 (a, b, c, d, e, f, g, h, i, j, k, l)) = T12 (negate a, negate b, negate c, negate d, negate e, negate f, negate g, negate h, negate i, negate j, negate k, negate l)
  (T12 (a, b, c, d, e, f, g, h, i, j, k, l)) + (T12 (a', b', c', d', e', f', g', h', i', j', k', l')) = T12 (a + a', b + b', c + c', d + d', e + e', f + f', g + g', h + h', i + i', j + j', k + k', l + l')

instance Ring.C a => Ring.C (Tuple12 a) where
  one = T12 (one, one, one, one, one, one, one, one, one, one, one, one)
  (T12 (a, b, c, d, e, f, g, h, i, j, k, l)) * (T12 (a', b', c', d', e', f', g', h', i', j', k', l')) = T12 (a * a', b * b', c * c', d * d', e * e', f * f', g * g', h * h', i * i', j * j', k * k', l * l')

instance Foldable Tuple12 where
  foldMap mf (T12 (a, b, c, d, e, f, g, h, i, j, k, l)) = mf a `mappend` mf b `mappend` mf c `mappend` mf d `mappend` mf e `mappend` mf f `mappend` mf g `mappend` mf h `mappend` mf i `mappend` mf j `mappend` mf k `mappend` mf l

instance Show a => Show (Tuple12 a) where
  show (T12 x) = show x

------
-- Testing Instances
------


--arb :: Arbitrary a => Gen a
--arb = arbitrary

--instance Arbitrary a => Arbitrary (Tuple6 a) where
--  arbitrary = t6 <$ arb <*> arb <*> arb <*> arb <*> arb <*> arb

--instance Arbitrary a => Arbitrary (Tuple8 a) where
--  arbitrary = t8 <$ arb <*> arb <*> arb <*> arb <*> arb <*> arb <*> arb <*> arb

--instance Arbitrary a => Arbitrary (Tuple12 a) where
--  arbitrary = t12 <$ arb <*> arb <*> arb <*> arb <*> arb <*> arb <*> arb <*> arb <*> arb <*> arb <*> arb <*> arb