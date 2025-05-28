{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      :  Tuple
-- Copyright   :  (c) Grant Goodman 2021
-- Description :  Implementation of modular arithmetic
-- License     :  MIT
-- Maintainer  :  zornslemonade@gmail.com
-- Portability :  Experimental
--
-- Defining type synonyms for ordered tuples which contain only one type
-- (Only the three needed for the CubeConfiguration type are defined)
module Tuple
  ( Indexable (..),
    Tuple6 (..),
    Tuple8 (..),
    Tuple12 (..),
    t6,
    t6FromList,
    t8,
    t8FromList,
    t12,
    t12FromList,
  )
where

import qualified Algebra.Additive as Additive
import qualified Algebra.Ring as Ring
import qualified Algebra.ToInteger as ToInteger
import Control.Applicative (Applicative (pure), (<$>), (<*>))
import Data.Foldable (Foldable (foldMap), toList)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid (Monoid (mappend), mempty)
import Data.Traversable (Traversable, traverse)
import NumericPrelude
import Action
import Permutation (Permutation, (?.), (?^))
import qualified Test.Tasty.QuickCheck as Q
import Data.Semigroup
import Data.Group

class (Foldable z) => Indexable z where
  -- \| Indexing
  infixl 9 *!
  (*!) :: z a -> Integer -> Maybe a

  -- \| Indexing with a default value
  index :: z a -> a -> Integer -> a
  index t a n = Data.Maybe.fromMaybe a (t *! n)

  {-# MINIMAL (*!) #-}


-- Newtype for sextuples

newtype Tuple6 a = T6 (M.Map Integer a) deriving (Eq, Ord)

t6 :: a -> a -> a -> a -> a -> a -> Tuple6 a
t6 a b c d e f = T6 $ M.fromDistinctAscList $ zip [1..6] [a,b,c,d,e,f]

t6FromList :: [a] -> Tuple6 a
t6FromList [a, b, c, d, e, f] = t6 a b c d e f


instance Functor Tuple6 where
  fmap :: (a -> b) -> Tuple6 a -> Tuple6 b
  fmap x (T6 m) = T6 $ fmap x m

instance Foldable Tuple6 where
  foldMap :: Monoid m => (a -> m) -> Tuple6 a -> m
  foldMap mf (T6 m) = foldMap mf m

instance Traversable Tuple6 where
  traverse :: Applicative f => (a -> f b) -> Tuple6 a -> f (Tuple6 b)
  traverse mf (T6 m) = T6 <$> traverse mf m

instance Applicative Tuple6 where
  pure :: a -> Tuple6 a
  pure = T6 . M.fromDistinctAscList . zip [1..6] . repeat

  (<*>) :: Tuple6 (a -> b) -> Tuple6 a -> Tuple6 b
  (T6 m) <*> (T6 n) = T6 $ M.fromAscList [(k, (m M.! k) (n M.! k)) | k <- [1..6]]

instance Additive.C a => Additive.C (Tuple6 a) where
  zero :: Additive.C a => Tuple6 a
  zero = pure zero

  negate :: Additive.C a => Tuple6 a -> Tuple6 a
  negate = (negate <$>)

  (+) :: Additive.C a => Tuple6 a -> Tuple6 a -> Tuple6 a
  (+) = (<*>) . ((+) <$>)

instance Ring.C a => Ring.C (Tuple6 a) where
  one :: Ring.C a => Tuple6 a
  one = pure one

  (*) :: Ring.C a => Tuple6 a -> Tuple6 a -> Tuple6 a
  (*) = (<*>) . ((*) <$>)

instance Semigroup a => Semigroup (Tuple6 a) where
  (<>) :: Semigroup a => Tuple6 a -> Tuple6 a -> Tuple6 a
  (<>) = (<*>) . ((<>) <$>)

instance Monoid a => Monoid (Tuple6 a) where
  mempty :: Monoid a => Tuple6 a
  mempty = pure mempty

instance Group a => Group (Tuple6 a) where
  invert :: Group a => Tuple6 a -> Tuple6 a
  invert = (invert <$>)


instance Show a => Show (Tuple6 a) where
  show :: Show a => Tuple6 a -> String
  show = show . toList

instance Action (Permutation Integer) (Tuple6 a) where
  (?*) :: Permutation Integer -> Tuple6 a -> Tuple6 a
  o ?* (T6 x) = T6 $ M.mapKeys (o ?.) x

instance Indexable Tuple6 where
  (*!) :: Tuple6 a -> Integer -> Maybe a
  (T6 x) *! n = M.lookup n x


-- Newtype for octuples

newtype Tuple8 a = T8 (M.Map Integer a) deriving (Eq, Ord)

t8 :: a -> a -> a -> a -> a -> a -> a -> a -> Tuple8 a
t8 a b c d e f g h = T8 $ M.fromAscList $ zip [1..8] [a,b,c,d,e,f,g,h]

t8FromList :: [a] -> Tuple8 a
t8FromList [a, b, c, d, e, f, g, h] = t8 a b c d e f g h

instance Functor Tuple8 where
  fmap :: (a -> b) -> Tuple8 a -> Tuple8 b
  fmap x (T8 m) = T8 $ fmap x m

instance Foldable Tuple8 where
  foldMap :: Monoid m => (a -> m) -> Tuple8 a -> m
  foldMap mf (T8 m) = foldMap mf m

instance Traversable Tuple8 where
  traverse :: Applicative f => (a -> f b) -> Tuple8 a -> f (Tuple8 b)
  traverse mf (T8 m) = T8 <$> traverse mf m

instance Applicative Tuple8 where
  pure :: a -> Tuple8 a
  pure = T8 . M.fromDistinctAscList . zip [1..8] . repeat

  (<*>) :: Tuple8 (a -> b) -> Tuple8 a -> Tuple8 b
  (T8 m) <*> (T8 n) = T8 $ M.fromAscList [(k, (m M.! k) (n M.! k)) | k <- [1..8]]

instance Additive.C a => Additive.C (Tuple8 a) where
  zero :: Additive.C a => Tuple8 a
  zero = pure zero

  negate :: Additive.C a => Tuple8 a -> Tuple8 a
  negate = (negate <$>)

  (+) :: Additive.C a => Tuple8 a -> Tuple8 a -> Tuple8 a
  (+) = (<*>) . ((+) <$>)

instance Ring.C a => Ring.C (Tuple8 a) where
  one :: Ring.C a => Tuple8 a
  one = pure one

  (*) :: Ring.C a => Tuple8 a -> Tuple8 a -> Tuple8 a
  (*) = (<*>) . ((*) <$>)

instance Semigroup a => Semigroup (Tuple8 a) where
  (<>) :: Semigroup a => Tuple8 a -> Tuple8 a -> Tuple8 a
  (<>) = (<*>) . ((<>) <$>)

instance Monoid a => Monoid (Tuple8 a) where
  mempty :: Monoid a => Tuple8 a
  mempty = pure mempty

instance Group a => Group (Tuple8 a) where
  invert :: Group a => Tuple8 a -> Tuple8 a
  invert = (invert <$>)


instance Show a => Show (Tuple8 a) where
  show :: Show a => Tuple8 a -> String
  show = show . toList


instance Action (Permutation Integer) (Tuple8 a) where
  (?*) :: Permutation Integer -> Tuple8 a -> Tuple8 a
  o ?* (T8 x) = T8 $ M.mapKeys (o ?.) x

instance Indexable Tuple8 where
  (*!) :: Tuple8 a -> Integer -> Maybe a
  (T8 x) *! n = M.lookup n x

-- Newtype for duodecuples

newtype Tuple12 a = T12 (M.Map Integer a) deriving (Eq, Ord)

t12 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Tuple12 a
t12 a b c d e f g h i j k l = T12 $ M.fromAscList $ zip [1..12] [a,b,c,d,e,f,g,h,i,j,k,l]

t12FromList :: [a] -> Tuple12 a
t12FromList [a, b, c, d, e, f, g, h, i, j, k, l] = t12 a b c d e f g h i j k l

instance Functor Tuple12 where
  fmap :: (a -> b) -> Tuple12 a -> Tuple12 b
  fmap x (T12 m) = T12 $ fmap x m

instance Foldable Tuple12 where
  foldMap :: Monoid m => (a -> m) -> Tuple12 a -> m
  foldMap mf (T12 m) = foldMap mf m

instance Traversable Tuple12 where
  traverse :: Applicative f => (a -> f b) -> Tuple12 a -> f (Tuple12 b)
  traverse mf (T12 m) = T12 <$> traverse mf m

instance Applicative Tuple12 where
  pure :: a -> Tuple12 a
  pure = T12 . M.fromDistinctAscList . zip [1..12] . repeat

  (<*>) :: Tuple12 (a -> b) -> Tuple12 a -> Tuple12 b
  (T12 m) <*> (T12 n) = T12 $ M.fromAscList [(k, (m M.! k) (n M.! k)) | k <- [1..12]]

instance Additive.C a => Additive.C (Tuple12 a) where
  zero :: Additive.C a => Tuple12 a
  zero = pure zero

  negate :: Additive.C a => Tuple12 a -> Tuple12 a
  negate = (negate <$>)

  (+) :: Additive.C a => Tuple12 a -> Tuple12 a -> Tuple12 a
  (+) = (<*>) . ((+) <$>)

instance Ring.C a => Ring.C (Tuple12 a) where
  one :: Ring.C a => Tuple12 a
  one = pure one

  (*) :: Ring.C a => Tuple12 a -> Tuple12 a -> Tuple12 a
  (*) = (<*>) . ((*) <$>)


instance Semigroup a => Semigroup (Tuple12 a) where
  (<>) :: Semigroup a => Tuple12 a -> Tuple12 a -> Tuple12 a
  (<>) = (<*>) . ((<>) <$>)

instance Monoid a => Monoid (Tuple12 a) where
  mempty :: Monoid a => Tuple12 a
  mempty = pure mempty

instance Group a => Group (Tuple12 a) where
  invert :: Group a => Tuple12 a -> Tuple12 a
  invert = (invert <$>)

instance Show a => Show (Tuple12 a) where
  show :: Show a => Tuple12 a -> String
  show = show . toList

instance Action (Permutation Integer) (Tuple12 a) where
  (?*) :: Permutation Integer -> Tuple12 a -> Tuple12 a
  o ?* (T12 x) = T12 $ M.mapKeys (o ?.) x

instance Indexable Tuple12 where
  (*!) :: Tuple12 a -> Integer -> Maybe a
  (T12 x) *! n = M.lookup n x

------
-- Testing Instances
------

arb :: Q.Arbitrary a => Q.Gen a
arb = Q.arbitrary

instance Q.Arbitrary a => Q.Arbitrary (Tuple6 a) where
  arbitrary :: Q.Arbitrary a => Q.Gen (Tuple6 a)
  arbitrary = t6 <$> arb <*> arb <*> arb <*> arb <*> arb <*> arb

instance Q.Arbitrary a => Q.Arbitrary (Tuple8 a) where
  arbitrary :: Q.Arbitrary a => Q.Gen (Tuple8 a)
  arbitrary = t8 <$> arb <*> arb <*> arb <*> arb <*> arb <*> arb <*> arb <*> arb

instance Q.Arbitrary a => Q.Arbitrary (Tuple12 a) where
  arbitrary :: Q.Arbitrary a => Q.Gen (Tuple12 a)
  arbitrary = t12 <$> arb <*> arb <*> arb <*> arb <*> arb <*> arb <*> arb <*> arb <*> arb <*> arb <*> arb <*> arb