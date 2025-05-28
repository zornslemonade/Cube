{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Skewb where

import qualified Algebra.Additive as Additive
import qualified Algebra.IntegralDomain as IntegralDomain
import qualified Algebra.Ring as Ring
import qualified Algebra.ToInteger as ToInteger
import qualified Algebra.ZeroTestable as ZeroTestable
import Data.Group
import qualified Data.List as L
import Data.Monoid
import Data.Semigroup
import Modular (Mod3, Mod4)
import Number.GaloisField2p32m5 (base)
import NumericPrelude
import Action
import Permutation hiding (i)
import qualified Permutation as P
import Tuple (Tuple6, Tuple8, t6, t8)
import TwistyPuzzle hiding (i)

type CenterP = Permutation Integer

type CornerP = Permutation Integer

type CenterO = Tuple6 Mod4

type VertexO = Tuple8 Mod3

newtype SkewbConfiguration = Skewb (CenterP, CornerP, CenterO, VertexO)

showSkewbConfig :: SkewbConfiguration -> String
showSkewbConfig (Skewb (a, b, xs, ys)) = L.intercalate "\n" [showInline a, showInline b, show xs, show ys]

instance Show SkewbConfiguration where
  show :: SkewbConfiguration -> String
  show = showSkewbConfig

------
-- Instantiating typeclasses
------

instance Semigroup SkewbConfiguration where
  (<>) :: SkewbConfiguration -> SkewbConfiguration -> SkewbConfiguration
  (Skewb (a1, b1, xs1, ys1)) <> (Skewb (a2, b2, xs2, ys2)) = Skewb (a, b, xs, ys)
    where
      a = a1 ? a2
      b = b1 ? b2
      xs = xs1 *? a2 + xs2
      ys = ys1 *? b2 + ys2

instance Monoid SkewbConfiguration where
  mempty :: SkewbConfiguration
  mempty = i

instance Group SkewbConfiguration where
  invert :: SkewbConfiguration -> SkewbConfiguration
  invert (Skewb (a, b, xs, ys)) = Skewb (a, b, xs, ys)
    where
      a' = invert a
      b' = invert b
      xs' = a ?* (-xs)
      ys' = b ?* (-ys)

------
-- Shorthand for the group operations
------

instance TwistyPuzzle SkewbConfiguration where
  (|#|) :: SkewbConfiguration -> SkewbConfiguration -> SkewbConfiguration
  x |#| y = x <> y

  (|#|^) :: (Eq b, IntegralDomain.C b, ZeroTestable.C b) => SkewbConfiguration -> b -> SkewbConfiguration
  x |#|^ 0 = i
  x |#|^ (-1) = invert x
  x |#|^ n
    | even n = (x |#| x) |#|^ div n 2
    | otherwise = x |#| (x |#| x) |#|^ div n 2

  (|#|^|#|) :: SkewbConfiguration -> SkewbConfiguration -> SkewbConfiguration
  x |#|^|#| y = invert y |#| x |#| y

  (>|#|<) :: SkewbConfiguration -> SkewbConfiguration -> SkewbConfiguration
  x >|#|< y = invert x |#| invert y |#| x |#| y

------
-- Explicitly writing out the cube configurations corresponding to the identity plus the single Turn of each face
------

-- Identity (no change)
i :: SkewbConfiguration
i = Skewb (P.i, P.i, 0, 0)

-- Corner 1: Upper Front Left (Clockwise)
ufl :: SkewbConfiguration
ufl = Skewb (p [[1, 2, 3]], p [[2, 4, 5]], t6 1 1 2 0 0 0, t8 1 2 0 2 2 0 0 0)

-- Corner 2: Upper Back Left (Clockwise)
ubl :: SkewbConfiguration
ubl = Skewb (p [[1, 3, 4]], p [[1, 8, 3]], t6 0 0 1 3 0 0, t8 2 1 2 0 0 0 0 2)

-- Corner 3: Upper Back Right (Clockwise)
ubr :: SkewbConfiguration
ubr = Skewb (p [[1, 4, 5]], p [[2, 7, 4]], t6 3 0 0 1 0 0, t8 0 2 1 2 0 0 2 0)

-- Corner 4: Upper Front Right (Clockwise)
ufr :: SkewbConfiguration
ufr = Skewb (p [[1, 5, 2]], p [[1, 3, 6]], t6 2 1 0 0 1 0, t8 2 0 2 1 0 2 0 0)

-- Corner 5: Lower Front Left (Clockwise)
dfl :: SkewbConfiguration
dfl = Skewb (p [[2, 6, 3]], p [[1, 6, 8]], t6 0 1 1 0 0 2, t8 2 0 0 0 1 0 2 2)

-- Corner 6: Lower Front Right (Clockwise)
dfr :: SkewbConfiguration
dfr = Skewb (p [[2, 5, 6]], p [[4, 7, 5]], t6 0 1 0 0 1 2, t8 0 0 0 2 2 1 2 0)

-- Corner 7: Lower Back Right (Clockwise)
dbr :: SkewbConfiguration
dbr = Skewb (p [[4, 6, 5]], p [[3, 8, 6]], t6 0 0 0 3 1 0, t8 0 0 2 0 0 2 1 2)

-- Corner 8: Lower Back Left (Clockwise)
dbl :: SkewbConfiguration
dbl = Skewb (p [[3, 6, 4]], p [[2, 5, 7]], t6 0 0 0 1 0 3, t8 0 2 0 0 2 0 2 1)

