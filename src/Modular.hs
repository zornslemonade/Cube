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

import Test.Tasty.QuickCheck

------
-- Defining modular arithmetic types
------

-- |A class for type representing modular arithmetic
class Num a => Modular a where
  unmod :: Integral b => a -> b
  modulus :: Integral b => a -> b

newtype Mod2 = M2 Integer deriving (Eq, Ord)

m2 :: Integral a => a -> Mod2
m2 = M2 . mod 2 . toInteger

instance Modular Mod2 where
  unmod (M2 x) = fromInteger x
  modulus = const 2

instance Num Mod2 where
  (M2 x) + (M2 y) = fromInteger (x + y)
  (M2 x) * (M2 y) = fromInteger (x * y)
  negate = id
  abs = id
  signum = const (M2 1)
  fromInteger n = M2 (n `mod` 2)

instance Show Mod2 where
  show (M2 x) = show x

newtype Mod3 = M3 Integer deriving (Eq, Ord)

m3 :: Integral a => a -> Mod3
m3 = M3 . mod 3 . toInteger

instance Modular Mod3 where
  unmod (M3 x) = fromInteger x
  modulus = const 3

instance Num Mod3 where
  (M3 x) + (M3 y) = fromInteger (x + y)
  (M3 x) * (M3 y) = fromInteger (x * y)
  negate (M3 x) = fromInteger (- x)
  abs = id
  signum = const (M3 1)
  fromInteger n = M3 (n `mod` 3)

instance Show Mod3 where
  show (M3 x) = show x

newtype Mod4 = M4 Integer deriving (Eq, Ord)

m4 :: Integral a => a -> Mod4
m4 = M4 . mod 4 . toInteger

instance Modular Mod4 where
  unmod (M4 x) = fromInteger x
  modulus = const 4

instance Num Mod4 where
  (M4 x) + (M4 y) = fromInteger (x + y)
  (M4 x) * (M4 y) = fromInteger (x * y)
  negate (M4 x) = fromInteger (- x)
  abs = id
  signum = const (M4 1)
  fromInteger n = M4 (n `mod` 4)

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
