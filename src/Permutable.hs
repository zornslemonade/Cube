module Permutable where

import Data.Foldable
import qualified Data.List as L
import qualified Data.Map as M
import Permutation

------
-- Defining an Permutable type class
------

-- This class is meant for container types which admit a well-defined action
-- by the symmetric group S_n, where n is their length.

class (Foldable z) => Permutable z where
  -- 1-based indexing
  infixl 9 *!
  (*!) :: Integral b => z a -> b -> a

  -- Inverse of toList
  fromList :: [a] -> z a

  -- Utility function
  toPairList :: Integral b => z a -> [(b, a)]
  toPairList = zip [1 ..] . toList

  -- Right and left action by permutation
  infixr 7 *?
  (*?) :: Integral b => z a -> Permutation b -> z a
  x *? o = (o ^- 1) ?* x

  infixl 7 ?*
  (?*) :: Integral b => Permutation b -> z a -> z a
  o ?* x = fromList $ M.elems $ M.fromList [(o ?. n, y) | (n, y) <- toPairList x]

------
-- Defining type synonyms for ordered tuples which contain only one type
-- (Only the three needed for the CubeConfiguration type are defined)
------

-- Newtype for sextuples

newtype Tuple6 a = T6 (a, a, a, a, a, a) deriving (Eq)

instance Permutable Tuple6 where
  fromList [a, b, c, d, e, f] = T6 (a, b, c, d, e, f)
  fromList _ = error "Tuple6: Given list does not have exactly 6 elements"

  (T6 (a, b, c, d, e, f)) *! n =
    case n of
      1 -> a
      2 -> b
      3 -> c
      4 -> d
      5 -> e
      6 -> f
      _ -> error "Tuple6: Tuple index out of bounds"

instance Num a => Num (Tuple6 a) where
  (T6 (a, b, c, d, e, f)) + (T6 (a', b', c', d', e', f')) = T6 (a + a', b + b', c + c', d + d', e + e', f + f')
  (T6 (a, b, c, d, e, f)) * (T6 (a', b', c', d', e', f')) = T6 (a * a', b * b', c * c', d * d', e * e', f * f')
  negate (T6 (a, b, c, d, e, f)) = T6 (-a, -b, -c, -d, -e, -f)
  abs (T6 (a, b, c, d, e, f)) = T6 (abs a, abs b, abs c, abs d, abs e, abs f)
  signum = error "(Tuple6).signum: not applicable"
  fromInteger n = T6 (fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n)

instance Foldable Tuple6 where
  foldMap mf (T6 (a, b, c, d, e, f)) = mf a `mappend` mf b `mappend` mf c `mappend` mf d `mappend` mf e `mappend` mf f

instance Show a => Show (Tuple6 a) where
  show (T6 x) = show x

-- Newtype for Octuples
newtype Tuple8 a = T8 (a, a, a, a, a, a, a, a) deriving (Eq)

instance Permutable Tuple8 where
  fromList [a, b, c, d, e, f, g, h] = T8 (a, b, c, d, e, f, g, h)
  fromList _ = error "Tuple8: Given list does not have exactly 8 elements"

  (T8 (a, b, c, d, e, f, g, h)) *! n =
    case n of
      1 -> a
      2 -> b
      3 -> c
      4 -> d
      5 -> e
      6 -> f
      7 -> g
      8 -> h
      _ -> error "Tuple8: Tuple index out of bounds"

instance Num a => Num (Tuple8 a) where
  (T8 (a, b, c, d, e, f, g, h)) + (T8 (a', b', c', d', e', f', g', h')) = T8 (a + a', b + b', c + c', d + d', e + e', f + f', g + g', h + h')
  (T8 (a, b, c, d, e, f, g, h)) * (T8 (a', b', c', d', e', f', g', h')) = T8 (a * a', b * b', c * c', d * d', e * e', f * f', g * g', h * h')
  negate (T8 (a, b, c, d, e, f, g, h)) = T8 (-a, -b, -c, -d, -e, -f, -g, -h)
  abs (T8 (a, b, c, d, e, f, g, h)) = T8 (abs a, abs b, abs c, abs d, abs e, abs f, abs g, abs h)
  signum = error "(Tuple8).signum: not applicable"
  fromInteger n = T8 (fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n)

instance Foldable Tuple8 where
  foldMap mf (T8 (a, b, c, d, e, f, g, h)) = mf a `mappend` mf b `mappend` mf c `mappend` mf d `mappend` mf e `mappend` mf f `mappend` mf g `mappend` mf h

instance Show a => Show (Tuple8 a) where
  show (T8 x) = show x

-- Newtype for Duodecuples

newtype Tuple12 a = T12 (a, a, a, a, a, a, a, a, a, a, a, a) deriving (Eq)

instance Permutable Tuple12 where
  fromList [a, b, c, d, e, f, g, h, i, j, k, l] = T12 (a, b, c, d, e, f, g, h, i, j, k, l)
  fromList _ = error "Tuple12: Given list does not have exactly 12 elements"

  (T12 (a, b, c, d, e, f, g, h, i, j, k, l)) *! n =
    case n of
      1 -> a
      2 -> b
      3 -> c
      4 -> d
      5 -> e
      6 -> f
      7 -> g
      8 -> h
      9 -> i
      10 -> j
      11 -> k
      12 -> l
      _ -> error "Tuple12: Tuple index out of bounds"

instance Num a => Num (Tuple12 a) where
  (T12 (a, b, c, d, e, f, g, h, i, j, k, l)) + (T12 (a', b', c', d', e', f', g', h', i', j', k', l')) = T12 (a + a', b + b', c + c', d + d', e + e', f + f', g + g', h + h', i + i', j + j', k + k', l + l')
  (T12 (a, b, c, d, e, f, g, h, i, j, k, l)) * (T12 (a', b', c', d', e', f', g', h', i', j', k', l')) = T12 (a * a', b * b', c * c', d * d', e * e', f * f', g * g', h * h', i * i', j * j', k * k', l * l')
  negate (T12 (a, b, c, d, e, f, g, h, i, j, k, l)) = T12 (-a, -b, -c, -d, -e, -f, -g, -h, -i, -j, -k, -l)
  abs (T12 (a, b, c, d, e, f, g, h, i, j, k, l)) = T12 (abs a, abs b, abs c, abs d, abs e, abs f, abs g, abs h, abs i, abs j, abs k, abs l)
  signum = error "(Tuple12).signum: not applicable"
  fromInteger n = T12 (fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n)

instance Foldable Tuple12 where
  foldMap mf (T12 (a, b, c, d, e, f, g, h, i, j, k, l)) = mf a `mappend` mf b `mappend` mf c `mappend` mf d `mappend` mf e `mappend` mf f `mappend` mf g `mappend` mf h `mappend` mf i `mappend` mf j `mappend` mf k `mappend` mf l

instance Show a => Show (Tuple12 a) where
  show (T12 x) = show x
