{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
  -- |
-- Module      :  Modular
-- Copyright   :  (c) Grant Goodman 2021
-- Description :  Implementation of modular arithmetic
-- License     :  MIT
-- Maintainer  :  zornslemonade@gmail.com
-- Portability :  Experimental
--
-- A module for working with modular arithmetic without GHC extensions.
-- Only supports modular arithmetic modulo 2, 3, and 4.
module Modular
  ( Modular (..),

    -- * Predefined types
    Mod2,
    Mod3,
    Mod4,

    -- * Constructors
    m2,
    m3,
    m4,
  )
where

import NumericPrelude
import Algebra.Ring as Ring
import Algebra.Additive as Additive
import Algebra.ToInteger as ToInteger
import Test.Tasty.QuickCheck

------
-- Defining modular arithmetic types
------

-- |A class for type representing modular arithmetic
class Ring.C a => Modular a where
  unmod :: Ring.C b => a -> b
  modulus :: Ring.C b => a -> b

newtype Mod2 = M2 Integer deriving (Eq, Ord)

m2 :: (Additive.C a, Ring.C a, ToInteger.C a) => a -> Mod2
m2 = M2 . (`mod` 2) . toInteger

instance Modular Mod2 where
  unmod (M2 x) = fromInteger x
  modulus = const 2

instance Additive.C Mod2 where
  zero = 0
  negate (M2 x) = fromInteger $ negate x
  (M2 x) + (M2 y) = fromInteger (x + y)

instance Ring.C Mod2 where
  one = fromInteger one
  (M2 x) * (M2 y) = fromInteger (x * y)

instance Show Mod2 where
  show (M2 x) = show x

newtype Mod3 = M3 Integer deriving (Eq, Ord)

m3 :: (Additive.C a, Ring.C a, ToInteger.C a) => a -> Mod3
m3 = M3 . (`mod` 3) . toInteger

instance Modular Mod3 where
  unmod (M3 x) = fromInteger x
  modulus = const 3

instance Additive.C Mod3 where
  zero = fromInteger zero
  negate (M3 x) = fromInteger $ negate x
  (M3 x) + (M3 y) = fromInteger (x + y)

instance Ring.C Mod3 where
  one = fromInteger one
  (M3 x) * (M3 y) = fromInteger (x * y)

instance Show Mod3 where
  show (M3 x) = show x

newtype Mod4 = M4 Integer deriving (Eq, Ord)

m4 :: (Additive.C a, Ring.C a, ToInteger.C a) => a -> Mod4
m4 = M4 . (`mod` 4) . toInteger

instance Modular Mod4 where
  unmod (M4 x) = fromInteger x
  modulus = const 4

instance Additive.C Mod4 where
  zero = zero
  negate = negate
  (M4 x) + (M4 y) = fromInteger (x + y)

instance Ring.C Mod4 where
  one = one
  (M4 x) * (M4 y) = fromInteger (x * y)

instance Show Mod4 where
  show (M4 x) = show x

------
-- Testing Instances
------

instance Arbitrary Mod2 where
  arbitrary = elements [0, 1]

instance Arbitrary Mod3 where
  arbitrary = elements [0, 1, 2]

instance Arbitrary Mod4 where
  arbitrary = elements [0, 1, 2, 3]
