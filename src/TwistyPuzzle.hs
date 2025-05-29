{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  Twisty Puzzle
-- Copyright   :  (c) Grant Goodman 2021
-- Description :  A typeclass for Twisty Puzzles
-- License     :  MIT
-- Maintainer  :  zornslemonade@gmail.com
-- Portability :  Experimental
--
-- A module for working with twisty puzzles
module TwistyPuzzle (TwistyPuzzle (..), TwistablePuzzle (..)) where

import Action
import qualified Algebra.IntegralDomain as IntegralDomain
import qualified Algebra.RealIntegral
import qualified Algebra.ZeroTestable as ZeroTestable
import Data.Bits (Bits (xor))
import Data.Foldable
import qualified Data.Group as G
import Data.Monoid (Monoid (mappend, mempty), (<>))
import Data.Semigroup
import Number.DimensionTerm.SI (yard)
import NumericPrelude
import qualified Permutation as P
import qualified Algebra.Ring as Ring

class (G.Group p, G.Group o, Action p o, Ord piece) => TwistyPuzzle config p o piece | config -> p o piece where
  getPositions :: config -> p

  getOrientations :: config -> o

  constructConfig :: p -> o -> config

  -- | Composition of puzzle configurations
  infixl 7 |#|

  (|#|) :: config -> config -> config
  x |#| y = constructConfig (xp <> yp) (xo *? yp <> yo)
    where
      xp = getPositions x
      xo = getOrientations x
      yp = getPositions y
      yo = getOrientations y

  solved :: config
  solved = constructConfig mempty mempty

  invert :: config -> config
  invert x = constructConfig (G.invert xp) (xp ?* G.invert xo)
    where
      xp = getPositions x
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

  -- | Returns information on the different types of pieces for the puzzle
  --
  -- [(piece constructor, # of positions, # of orientations)]
  getPieceData :: config -> [(Integer -> Integer -> piece, Integer, Integer)]

  permutePiece :: config -> piece -> piece

  toPermutation :: config -> P.Permutation piece
  toPermutation z = P.pp pieces
    where
      pieces = concatMap permutePieces $ getPieceData z
      permutePieces (pieceConstructor, n', k') = [(pieceConstructor n k, permutePiece z $ pieceConstructor n k) | n <- [1..n'], k <- [0..k'-1]]

  fromPermutation :: P.Permutation piece -> Maybe config

  order :: config -> Int
  order = P.order . toPermutation

  {-# MINIMAL getPositions, getOrientations, constructConfig, getPieceData, permutePiece, fromPermutation #-}

class TwistyPuzzle config p o piece => TwistablePuzzle config p o piece turn | config -> turn where
  applyTurn :: turn -> config -> config

  applyTurns :: Foldable z => config -> z turn -> config
  applyTurns = foldr applyTurn

  {-# MINIMAL applyTurn #-}
