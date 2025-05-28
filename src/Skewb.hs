{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
    ( filter,
      fst,
      map,
      ($),
      Eq,
      Ord,
      Show(show),
      Bool(..),
      String,
      Integer,
      Maybe(..),
      uncurry,
      all,
      (.),
      (+),
      fromInteger,
      ifThenElse )
import Action
import Permutation hiding (i)
import qualified Permutation as P
import Tuple
import TwistyPuzzle hiding (i)
import qualified Data.Function as F
import Control.Applicative

type CenterP = Permutation Integer

type VertexP = Permutation Integer

type CenterO = Tuple6 Mod4

type VertexO = Tuple8 Mod3

newtype SkewbConfiguration = Skewb (CenterP, VertexP, CenterO, VertexO)

showSkewbConfig :: SkewbConfiguration -> String
showSkewbConfig (Skewb (a, b, xs, ys)) = L.intercalate "\n" [showInline a, showInline b, show xs, show ys]

instance Show SkewbConfiguration where
  show :: SkewbConfiguration -> String
  show = showSkewbConfig

-- | Type for representing individual pieces
data Skewbie = C Integer Mod4 | V Integer Mod3 deriving (Eq, Ord, Show)

isCenterSkewbie :: Skewbie -> Bool
isCenterSkewbie (C _ _) = True
isCenterSkewbie _ = False

isVertexSkewbie :: Skewbie -> Bool
isVertexSkewbie (V _ _) = True
isVertexSkewbie _ = False

getSkewbieNumber :: Skewbie -> Integer
getSkewbieNumber (C n _) = n
getSkewbieNumber (V n _) = n

------
-- Instantiating typeclasses
------

instance TwistyPuzzle SkewbConfiguration (CenterP, VertexP) (CenterO, VertexO) Skewbie where
  getPositions :: SkewbConfiguration -> (CenterP, VertexP)
  getPositions (Skewb (a, b, xs, ys)) = (a, b)

  getOrientations :: SkewbConfiguration -> (CenterO, VertexO)
  getOrientations (Skewb (a, b, xs, ys)) = (xs, ys)

  constructConfig :: (CenterP, VertexP) -> (CenterO, VertexO) -> SkewbConfiguration
  constructConfig (a, b) (xs, ys) = Skewb (a, b, xs, ys)

  -- \|
  -- Configurations of the Skewb can also be seen as permutations of the set of stickers (where the 4 orientations of each center
  -- Skewbie sticker are considered distinct).
  -- This manifests as a monomorphism from the group of Skewb configurations into the permutation group of stickers.
  -- Conversely, not every permutation of stickers gives a valid configuration of the Skewb, for example a vertex sticker can never
  -- end up in the place of an edge sticker.
  --
  -- This sends a configuration to a permutation of the stickers, where each sticker is represented as a tuple (X, n, m), where
  -- X encodes whether it is a center, edge, or vertex Skewbie (taking the values 'C', 'E', or 'V', respectively), n represents the Skewbie
  -- the sticker is attached to, and m represents the face of that Skewbie that the sticker is attached to.
  -- For center Skewbies, m represents the orientation of the sticker.
  toPermutation :: SkewbConfiguration -> Permutation Skewbie
  toPermutation (Skewb (a, b, xs, ys)) = a' ? b'
    where
      t *!! n = index t 0 n
      a' = pp [(C n k, C (a ?. n) (xs *!! n + k)) | n <- [1 .. 6], k <- [0, 1, 2, 3]]
      b' = pp [(V n k, V (b ?. n) (ys *!! n + k)) | n <- [1 .. 8], k <- [0, 1, 2]]

  fromPermutation :: Permutation Skewbie -> Maybe SkewbConfiguration
  fromPermutation o = if all staysSame (fst <$> toPairs o) then Just $ Skewb (a, b, xs, ys) else Nothing
    where
      staysSame skewbie = case skewbie of
        C _ _ -> isCenterSkewbie (o ?. skewbie)
        V _ _ -> isVertexSkewbie (o ?. skewbie)
      getSkewbieNumbers = uncurry ((,) `F.on` getSkewbieNumber)
      a = pp $ map getSkewbieNumbers $ filter (isCenterSkewbie . fst) $ toPairs o
      b = pp $ map getSkewbieNumbers $ filter (isVertexSkewbie . fst) $ toPairs o
      xs = t6FromList [case o ?. C n 0 of C _ m -> m; _ -> 0 | n <- [1 .. 6]]
      ys = t8FromList [case o ?. V n 0 of V _ m -> m; _ -> 0 | n <- [1 .. 8]]

------
-- Explicitly writing out the Skewb configurations corresponding to the identity plus the single Turn of each face
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

