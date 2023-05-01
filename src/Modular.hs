{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
    Mod2 (..),
    Mod3 (..),
    Mod4 (..),

    -- * Extra constructors
    m2,
    m3,
    m4
  )
where

import Algebra.Additive as Additive
import Algebra.Ring as Ring
import Algebra.ToInteger as ToInteger
import NumericPrelude
import Test.Tasty.QuickCheck

------
-- Defining modular arithmetic types
------

-- | A class for type representing modular arithmetic
class Ring.C a => Modular a where
  unmod :: (Ring.C b) => a -> b
  modulus :: a -> Integer

------
-- Mod 2
------

newtype Mod2 = M2 Integer deriving (Eq, Ord)

m2 :: Integer -> Mod2
m2 = M2 . (`mod` 2)

instance Additive.C Mod2 where
  zero :: Mod2
  zero = 0

  negate :: Mod2 -> Mod2
  negate (M2 x) = m2 $ negate x

  (+) :: Mod2 -> Mod2 -> Mod2
  (M2 x) + (M2 y) = m2 $ x + y

instance Ring.C Mod2 where
  fromInteger :: Integer -> Mod2
  fromInteger = m2

  (*) :: Mod2 -> Mod2 -> Mod2
  (M2 x) * (M2 y) = m2 $ x * y

instance Modular Mod2 where
  unmod :: (Ring.C b) => Mod2 -> b
  unmod (M2 x) = fromInteger x

  modulus :: Mod2 -> Integer
  modulus = const 2

instance Show Mod2 where
  show :: Mod2 -> String
  show (M2 x) = show x

------
-- Mod 3
------

newtype Mod3 = M3 Integer deriving (Eq, Ord)

m3 :: Integer -> Mod3
m3 = M3 . (`mod` 3)

instance Additive.C Mod3 where
  zero :: Mod3
  zero = 0

  negate :: Mod3 -> Mod3
  negate (M3 x) = m3 $ negate x

  (+) :: Mod3 -> Mod3 -> Mod3
  (M3 x) + (M3 y) = m3 $ x + y

instance Ring.C Mod3 where
  fromInteger :: Integer -> Mod3
  fromInteger = m3

  (*) :: Mod3 -> Mod3 -> Mod3
  (M3 x) * (M3 y) = m3 $ x * y

instance Modular Mod3 where
  unmod :: (Ring.C b) => Mod3 -> b
  unmod (M3 x) = fromInteger x

  modulus :: Mod3 -> Integer
  modulus = const 3

instance Show Mod3 where
  show :: Mod3 -> String
  show (M3 x) = show x

------
-- Mod 4
------

newtype Mod4 = M4 Integer deriving (Eq, Ord)

m4 :: Integer -> Mod4
m4 = M4 . (`mod` 4)

instance Additive.C Mod4 where
  zero :: Mod4
  zero = 0

  negate :: Mod4 -> Mod4
  negate (M4 x) = m4 $ negate x

  (+) :: Mod4 -> Mod4 -> Mod4
  (M4 x) + (M4 y) = m4 $ x + y

instance Ring.C Mod4 where
  fromInteger :: Integer -> Mod4
  fromInteger = m4

  (*) :: Mod4 -> Mod4 -> Mod4
  (M4 x) * (M4 y) = m4 $ x * y

instance Modular Mod4 where
  unmod :: (Ring.C b) => Mod4 -> b
  unmod (M4 x) = fromInteger x

  modulus :: Mod4 -> Integer
  modulus = const 4

instance Show Mod4 where
  show :: Mod4 -> String
  show (M4 x)= show x

------
-- Testing Instances
------

instance Arbitrary Mod2 where
  arbitrary :: Gen Mod2
  arbitrary = elements [0, 1]

instance Arbitrary Mod3 where
  arbitrary :: Gen Mod3
  arbitrary = elements [0, 1, 2]

instance Arbitrary Mod4 where
  arbitrary :: Gen Mod4
  arbitrary = elements [0, 1, 2, 3]
