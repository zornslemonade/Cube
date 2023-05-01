{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  Permutable
-- Copyright   :  (c) Grant Goodman 2021
-- Description :  Implementation of the Generalized Symmetric Group
-- License     :  MIT
-- Maintainer  :  zornslemonade@gmail.com
-- Portability :  Experimental
--
-- A module which implements the generalized symmetric group for a few specific orders.
module Permutable
  ( Indexable (..),
    Permutable (..),
  )
where

import Control.Applicative (Applicative (pure), (<$>), (<*>))
import Data.Foldable (Foldable (foldMap), toList)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Traversable (Traversable, traverse)
import NumericPrelude
import Permutation (Permutation, (?.), (?^))
import qualified Test.Tasty.QuickCheck as Q

------
-- Defining an Permutable type class
------

class (Foldable z) => Indexable z where
  -- \| Indexing
  infixl 9 *!
  (*!) :: z a -> Integer -> Maybe a
  (*!) = flip M.lookup . toMap

  -- \| Unsafe Indexing
  infixl 9 *!!
  (*!!) :: z a -> Integer -> a
  x *!! n = fromJust $ x *! n

  -- \| Label each element with its index
  labelIndices :: z a -> z (Integer, a)

  size :: z a -> Int
  size = length . toList

  -- \| Converting to a map
  toMap :: z a -> M.Map Integer a
  toMap x = M.fromList $ toList $ labelIndices x

  {-# MINIMAL labelIndices #-}

-- This class is meant for container types which admit a well-defined action
-- by the symmetric group S_n, where n is their length.
class (Indexable z) => Permutable z where
  -- \| Left action by permutation
  infixr 7 *?
  (*?) :: z a -> Permutation Integer -> z a
  x *? o = o ?^ (-1) ?* x

  -- \| Right action by permutation
  infixr 7 ?*
  (?*) :: Permutation Integer -> z a -> z a
  o ?* x = x *? o ?^ (-1)

  {-# MINIMAL (*?) | (?*) #-}