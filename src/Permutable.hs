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
  ( Permutable (..),
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

------
-- Defining an Permutable type class
------

-- This class is meant for container types which admit a well-defined action
-- by the symmetric group S_n, where n is their length.
class Permutable z where
  -- \| Left action by permutation
  infixr 7 *?
  (*?) :: z a -> Permutation Integer -> z a
  x *? o = o ?^ (-1) ?* x

  -- \| Right action by permutation
  infixr 7 ?*
  (?*) :: Permutation Integer -> z a -> z a
  o ?* x = x *? o ?^ (-1)

  {-# MINIMAL (*?) | (?*) #-}