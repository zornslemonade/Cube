{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
  ( Tuple6 (..),
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
import Data.Maybe (fromJust)
import Data.Monoid (Monoid (mappend))
import Data.Traversable (Traversable, traverse)
import NumericPrelude
import Permutable
import Permutation (Permutation, (?.), (?^))
import qualified Test.Tasty.QuickCheck as Q

-- Newtype for sextuples

newtype Tuple6 a = T6 (a, a, a, a, a, a) deriving (Eq, Ord)

t6 :: a -> a -> a -> a -> a -> a -> Tuple6 a
t6 = (((((T6 .) .) .) .) .) . (,,,,,)

t6FromList :: [a] -> Tuple6 a
t6FromList [a, b, c, d, e, f] = T6 (a, b, c, d, e, f)

instance Functor Tuple6 where
  fmap :: (a -> b) -> Tuple6 a -> Tuple6 b
  fmap x (T6 (a, b, c, d, e, f)) = T6 (x a, x b, x c, x d, x e, x f)

instance Applicative Tuple6 where
  pure :: a -> Tuple6 a
  pure x = T6 (x, x, x, x, x, x)

  (<*>) :: Tuple6 (a -> b) -> Tuple6 a -> Tuple6 b
  (T6 (xa, xb, xc, xd, xe, xf)) <*> (T6 (a, b, c, d, e, f)) = T6 (xa a, xb b, xc c, xd d, xe e, xf f)

instance Foldable Tuple6 where
  foldMap :: Monoid m => (a -> m) -> Tuple6 a -> m
  foldMap mf (T6 (a, b, c, d, e, f)) = mf a `mappend` mf b `mappend` mf c `mappend` mf d `mappend` mf e `mappend` mf f

instance Traversable Tuple6 where
  traverse :: Applicative f => (a -> f b) -> Tuple6 a -> f (Tuple6 b)
  traverse mf (T6 (a, b, c, d, e, f)) = t6 <$> mf a <*> mf b <*> mf c <*> mf d <*> mf e <*> mf f

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

instance Show a => Show (Tuple6 a) where
  show :: Show a => Tuple6 a -> String
  show (T6 x) = show x

instance Indexable Tuple6 where
  (*!) :: Tuple6 a -> Integer -> Maybe a
  (T6 (a, b, c, d, e, f)) *! n =
    case n of
      1 -> Just a
      2 -> Just b
      3 -> Just c
      4 -> Just d
      5 -> Just e
      6 -> Just f
      _ -> Nothing

  size :: Tuple6 a -> Int
  size = const 6

  labelIndices :: Tuple6 a -> Tuple6 (Integer, a)
  labelIndices = (<*>) ((,) <$> T6 (1, 2, 3, 4, 5, 6))

instance Permutable Tuple6 where
  (?*) :: Permutation Integer -> Tuple6 a -> Tuple6 a
  o ?* x =
    let y = labelIndices x
        z = (fst <$> y)
     in (M.findWithDefault <$> x) <*> z <*> pure (M.mapKeys (o ?.) $ M.fromList $ toList y)

-- Newtype for octuples

newtype Tuple8 a = T8 (a, a, a, a, a, a, a, a) deriving (Eq, Ord)

t8 :: a -> a -> a -> a -> a -> a -> a -> a -> Tuple8 a
t8 = (((((((T8 .) .) .) .) .) .) .) . (,,,,,,,)

t8FromList :: [a] -> Tuple8 a
t8FromList [a, b, c, d, e, f, g, h] = T8 (a, b, c, d, e, f, g, h)

instance Functor Tuple8 where
  fmap :: (a -> b) -> Tuple8 a -> Tuple8 b
  fmap x (T8 (a, b, c, d, e, f, g, h)) = T8 (x a, x b, x c, x d, x e, x f, x g, x h)

instance Applicative Tuple8 where
  pure :: a -> Tuple8 a
  pure x = T8 (x, x, x, x, x, x, x, x)

  (<*>) :: Tuple8 (a -> b) -> Tuple8 a -> Tuple8 b
  (T8 (xa, xb, xc, xd, xe, xf, xg, xh)) <*> (T8 (a, b, c, d, e, f, g, h)) = T8 (xa a, xb b, xc c, xd d, xe e, xf f, xg g, xh h)

instance Foldable Tuple8 where
  foldMap :: Monoid m => (a -> m) -> Tuple8 a -> m
  foldMap m (T8 (a, b, c, d, e, f, g, h)) = m a `mappend` m b `mappend` m c `mappend` m d `mappend` m e `mappend` m f `mappend` m g `mappend` m h

instance Traversable Tuple8 where
  traverse :: Applicative f => (a -> f b) -> Tuple8 a -> f (Tuple8 b)
  traverse m (T8 (a, b, c, d, e, f, g, h)) = t8 <$> m a <*> m b <*> m c <*> m d <*> m e <*> m f <*> m g <*> m h

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

instance Show a => Show (Tuple8 a) where
  show :: Show a => Tuple8 a -> String
  show (T8 x) = show x

instance Indexable Tuple8 where
  (*!) :: Tuple8 a -> Integer -> Maybe a
  (T8 (a, b, c, d, e, f, g, h)) *! n =
    case n of
      1 -> Just a
      2 -> Just b
      3 -> Just c
      4 -> Just d
      5 -> Just e
      6 -> Just f
      7 -> Just g
      8 -> Just h
      _ -> Nothing

  size :: Tuple8 a -> Int
  size = const 8

  labelIndices :: Tuple8 a -> Tuple8 (Integer, a)
  labelIndices = (<*>) ((,) <$> T8 (1, 2, 3, 4, 5, 6, 7, 8))

instance Permutable Tuple8 where
  (?*) :: Permutation Integer -> Tuple8 a -> Tuple8 a
  o ?* x =
    let y = labelIndices x
        z = (fst <$> y)
     in (M.findWithDefault <$> x) <*> z <*> pure (M.mapKeys (o ?.) $ M.fromList $ toList y)

-- Newtype for duodecuples

newtype Tuple12 a = T12 (a, a, a, a, a, a, a, a, a, a, a, a) deriving (Eq, Ord)

t12 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Tuple12 a
t12 = (((((((((((T12 .) .) .) .) .) .) .) .) .) .) .) . (,,,,,,,,,,,)

t12FromList :: [a] -> Tuple12 a
t12FromList [a, b, c, d, e, f, g, h, i, j, k, l] = T12 (a, b, c, d, e, f, g, h, i, j, k, l)

instance Functor Tuple12 where
  fmap :: (a -> b) -> Tuple12 a -> Tuple12 b
  fmap x (T12 (a, b, c, d, e, f, g, h, i, j, k, l)) = T12 (x a, x b, x c, x d, x e, x f, x g, x h, x i, x j, x k, x l)

instance Applicative Tuple12 where
  pure :: a -> Tuple12 a
  pure x = T12 (x, x, x, x, x, x, x, x, x, x, x, x)

  (<*>) :: Tuple12 (a -> b) -> Tuple12 a -> Tuple12 b
  (T12 (xa, xb, xc, xd, xe, xf, xg, xh, xi, xj, xk, xl)) <*> (T12 (a, b, c, d, e, f, g, h, i, j, k, l)) = T12 (xa a, xb b, xc c, xd d, xe e, xf f, xg g, xh h, xi i, xj j, xk k, xl l)

instance Foldable Tuple12 where
  foldMap :: Monoid m => (a -> m) -> Tuple12 a -> m
  foldMap m (T12 (a, b, c, d, e, f, g, h, i, j, k, l)) = m a `mappend` m b `mappend` m c `mappend` m d `mappend` m e `mappend` m f `mappend` m g `mappend` m h `mappend` m i `mappend` m j `mappend` m k `mappend` m l

instance Traversable Tuple12 where
  traverse :: Applicative f => (a -> f b) -> Tuple12 a -> f (Tuple12 b)
  traverse m (T12 (a, b, c, d, e, f, g, h, i, j, k, l)) = t12 <$> m a <*> m b <*> m c <*> m d <*> m e <*> m f <*> m g <*> m h <*> m i <*> m j <*> m k <*> m l

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

instance Show a => Show (Tuple12 a) where
  show :: Show a => Tuple12 a -> String
  show (T12 x) = show x

instance Indexable Tuple12 where
  (*!) :: Tuple12 a -> Integer -> Maybe a
  (T12 (a, b, c, d, e, f, g, h, i, j, k, l)) *! n =
    case n of
      1 -> Just a
      2 -> Just b
      3 -> Just c
      4 -> Just d
      5 -> Just e
      6 -> Just f
      7 -> Just g
      8 -> Just h
      9 -> Just i
      10 -> Just j
      11 -> Just k
      12 -> Just l
      _ -> Nothing

  size :: Tuple12 a -> Int
  size = const 12

  labelIndices :: Tuple12 a -> Tuple12 (Integer, a)
  labelIndices = (<*>) ((,) <$> T12 (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))

instance Permutable Tuple12 where
  (?*) :: Permutation Integer -> Tuple12 a -> Tuple12 a
  o ?* x =
    let y = labelIndices x
        z = (fst <$> y)
     in (M.findWithDefault <$> x) <*> z <*> pure (M.mapKeys (o ?.) $ M.fromList $ toList y)

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