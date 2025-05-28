{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

-- |
-- Module      :  Action
-- Copyright   :  (c) Grant Goodman 2021
-- Description :  Implementation of the Generalized Symmetric Group
-- License     :  MIT
-- Maintainer  :  zornslemonade@gmail.com
-- Portability :  Experimental
--
-- A module which implements the generalized symmetric group for a few specific orders.
module Action
  ( Action (..),
  )
where

import Control.Applicative (Applicative (pure), (<$>), (<*>))
import Data.Foldable (Foldable (foldMap), toList)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Traversable (Traversable, traverse)
import NumericPrelude
import Permutation (Permutation, (?.), (?^))
import qualified Test.Tasty.QuickCheck as Q
import Data.Group

------
-- Defining an Permutable type class
------

-- This class is meant for container types which admit a well-defined action
-- by the symmetric group S_n, where n is their length.
class (Group g) => Action g z where
  -- \| Left action by a group element
  infixr 7 *?
  (*?) :: z -> g -> z
  x *? o = invert o ?* x

  -- \| Right action by group element
  infixr 7 ?*
  (?*) :: g -> z -> z
  o ?* x = x *? invert o

  {-# MINIMAL (*?) | (?*) #-}

instance (Action g1 z1, Action g2 z2) => Action (g1, g2) (z1, z2) where
  (*?) :: (Action g1 z1, Action g2 z2) => (z1, z2) -> (g1, g2) -> (z1, z2)
  (x1, x2) *? (o1, o2) = (x1 *? o1, x2 *? o2)

instance (Action g1 z1, Action g2 z2, Action g3 z3) => Action (g1, g2, g3) (z1, z2, z3) where
  (*?) :: (Action g1 z1, Action g2 z2, Action g3 z3) => (z1, z2, z3) -> (g1, g2, g3) -> (z1, z2, z3)
  (x1, x2, x3) *? (o1, o2, o3) = (x1 *? o1, x2 *? o2, x3 *? o3)