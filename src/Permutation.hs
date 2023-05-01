{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  Permutation
-- Copyright   :  (c) Grant Goodman 2021
-- Description :  Implementation of permutations
-- License     :  MIT
-- Maintainer  :  zornslemonade@gmail.com
-- Portability :  Experimental
--
-- A module for working with permutations. A permutation is a bijection from
-- a set onto itself. Permutations can be made from any type, so long as it
-- is an instance of Ord. This is because permutations are stored using
-- Data.Map.
module Permutation
  ( Permutation,

    -- * Construction
    p,
    pp,

    -- * Displaying permutations
    showInline,
    showMultiline,

    -- * Basic operations and notation for permutations
    i,
    (?.),
    (?-),
    (?),
    (?^),
    (?^?),
    (>?<),

    -- * Useful mathematical functions of permutations
    parity,
    sgn,
    order,
    subgroupOrder,
    cycleOf,
    support,
    transpositionDecomposition,
    threeCycleDecomposition,

    -- * Utility functions
    fromPairs,
    toPairs,
    fromCycles,
    toCycles,
    toFunction,

    -- * Functions for testing purposes
    permutationOf,
  )
where

import qualified Algebra.Additive as Additive
import qualified Algebra.IntegralDomain as IntegralDomain
import qualified Algebra.Ring as Ring
import qualified Algebra.ToInteger as ToInteger
import qualified Algebra.ZeroTestable as ZeroTestable
import Data.Functor ((<$>))
import Data.Group (Group (invert))
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Monoid (Monoid (..), (<>))
import Data.Semigroup (Semigroup)
import Modular
import NumericPrelude
import qualified Test.Tasty.QuickCheck as Q

------
-- Permutations are bijections, thought of with a multiplicative group structure which acts from the left on the underlying set
------

-- Permutations are stored as maps
newtype Permutation a = P (M.Map a a) deriving (Eq)

-- | Identity permutation
i :: Permutation a
i = P M.empty

-- | Construct a permutation from a list of cycles.
--  On invalid input, returns the identity.
p :: Ord a => [[a]] -> Permutation a
p cs = fromMaybe i $ fromCycles cs

-- | Construct a permutation from a list of pairs.
--  On invalid input, returns the identity.
pp :: Ord a => [(a, a)] -> Permutation a
pp zs = fromMaybe i $ fromPairs zs

------
-- Instantiating type classes
------

instance Ord a => Semigroup (Permutation a) where
  -- \| Composing permutations
  (<>) :: Ord a => Permutation a -> Permutation a -> Permutation a
  o <> q = pp [(x, o ?. q ?. x) | x <- support o ++ support q]

instance Ord a => Monoid (Permutation a) where
  mempty :: Ord a => Permutation a
  mempty = i

instance Ord a => Group (Permutation a) where
  invert :: Ord a => Permutation a -> Permutation a
  invert (P g) = P $ M.fromList $ map (\(x, y) -> (y, x)) $ M.toList g

-- | Converts the permutation to a string, showing its cycle decomposition on a single line, i.e.,
--
--  > (1 2 3)(4 5)
showInline :: (Ord a, Show a) => Permutation a -> String
showInline g
  | g == i = "1"
  | otherwise = concatMap showCycle $ toCycles g
  where
    showCycle xs = "(" ++ unwords (map show xs) ++ ")"

-- | Inserts a newline character between each cycle.
showMultiline :: (Ord a, Show a) => Permutation a -> String
showMultiline g
  | g == i = "1"
  | otherwise = L.intercalate "\n" (map showCycle (toCycles g))
  where
    showCycle xs = "(" ++ unwords (map show xs) ++ ")"

-- | Uses 'showMultiline'.
instance (Ord a, Show a) => Show (Permutation a) where
  show :: (Ord a, Show a) => Permutation a -> String
  show = showMultiline

------
-- Useful mathematical functions and notation for permutations
------

-- | Action on an element (Apply the permutation)
--
-- >>> p [[1,2,3]] ?. 2
-- 3
infixr 1 ?.

(?.) :: Ord a => Permutation a -> a -> a
o ?. x = toFunction o x

-- | Action on a set
--
-- >>> p [[1,2,3]] ?- [2,4]
-- [3,4]
infixr 1 ?-

(?-) :: Ord a => Permutation a -> [a] -> [a]
o ?- xs = [o ?. x | x <- xs]

-- | Multiplication
--
-- > (o ? q) ?. x == o ?. (q ?. x)
infixl 7 ?

(?) :: (Ord a) => Permutation a -> Permutation a -> Permutation a
(?) = mappend

-- | Exponentiation (including negative exponents)
--
-- >>> p [[1,2,3,4,5]] ?^ 2
-- (1 3 5 2 4)
infixl 8 ?^

(?^) :: Ord a => Permutation a -> Int -> Permutation a
x ?^ 0 = i
x ?^ (-1) = invert x
x ?^ n
  | even n = (x ? x) ?^ div n 2
  | otherwise = x ? (x ? x) ?^ div n 2

-- | Conjugation of two permutations, i.e.,
--
-- > o ?^? q == q ?^ (-1) ? o ? q
infix 8 ?^?

(?^?) :: Ord a => Permutation a -> Permutation a -> Permutation a
o ?^? q = (q ?^ (-1)) ? o ? q

-- | Commutator of two perumutations, i.e.,
--
-- > o >?< q == o ?^ (-1) ? q ?^ (-1) ? o ? q
infix 7 >?<

(>?<) :: Ord a => Permutation a -> Permutation a -> Permutation a
o >?< q = o ?^ (-1) ? q ?^ (-1) ? o ? q

-- | Parity of a permutation
parity :: Ord a => Permutation a -> Mod2
parity o = m2 $ toInteger $ foldr ((+) . (+ 1) . length) 0 (toCycles o)

-- | Sign of a permutation
sgn :: Ord a => Permutation a -> Int
sgn = ((-1) ^) . unmod . parity

-- | Compute the order of a permutation.
order :: Ord a => Permutation a -> Int
order = foldr (lcm . length) 1 . toCycles

-- | Compute the order of the group generated by a list of permutations.
subgroupOrder :: Ord a => [Permutation a] -> Int
subgroupOrder = foldr (lcm . order) 1

-- | Find the cycle of the given element in the given permutation.
cycleOf :: Ord a => a -> Permutation a -> [a]
cycleOf x o = cycleThrough x
  where
    cycleThrough y = let y' = o ?. y in if y' == x then [y] else y : cycleThrough y'

-- | The support of a permutation (the set of non-fixed points).
--  Works for permutations created using the supplied constructors.
support :: Ord a => Permutation a -> [a]
support (P g) = M.keys g

-- | Given z, decompose the permutation into an equivalent product of transpositions of the form (z x).
--  This can be done for any permutation and any z but is not unique.
--
--  E.g.,
--
--  >>> transpositionDecomposition 1 (p [[1,2,3],[4,5]])
-- [[1,3],[1,2],[1,4],[1,5],[1,4]]
transpositionDecomposition :: Ord a => a -> Permutation a -> [[a]]
transpositionDecomposition z o = concatMap cycleToTranspositions $ toCycles o
  where
    cycleToTranspositions s =
      case splitAt z s of
        ([], []) -> []
        (a : as, []) -> [z, a] : createTranspositions (a : as)
        (as, _ : bs) -> createTranspositions as ++ createTranspositions bs
    splitAt x [] = ([], [])
    splitAt x (y : ys)
      | x == y = ([], y : ys)
      | otherwise = let (a, b) = splitAt x ys in (y : a, b)
    createTranspositions [] = []
    createTranspositions (x : xs) = createTranspositions xs ++ [[z, x]]

-- | Given z1, z2, decompose the permutation into an equivalent product of 3-cycles of the form (z1 z2 x) and (z2 z1 x).
-- This can be done for any even permutation and any z1 and z2 but is not unique.
-- If the permutation is not even, the product will end in a transposition of the form (z1 x).
--
-- E.g.,
--
-- >>> threeCycleDecomposition 5 6 (p [[1,2],[3,4]])
-- [[6,5,2],[6,5,1],[5,6,2],[6,5,3],[6,5,1],[6,5,4],[5,6,3]]
threeCycleDecomposition :: Ord a => a -> a -> Permutation a -> [[a]]
threeCycleDecomposition z1 z2 o = shrink $ pairOff $ transpositionDecomposition z1 o
  where
    pairOff ([_, a] : [_, b] : xs)
      | a == b = pairOff xs
      | a == z2 = [z2, z1, b] : pairOff xs
      | b == z2 = [z1, z2, a] : pairOff xs
      | otherwise = [[z2, z1, b], [z2, z1, a], [z1, z2, b]] ++ pairOff xs
    pairOff xs = xs
    shrink ([a, b, c] : [e, f, g] : xs)
      | a == e && c == g = shrink $ [b, a, c] : xs
      | a == f && c == g = shrink xs
      | otherwise = [a, b, c] : shrink ([e, f, g] : xs)
    shrink xs = xs

------
-- Utility Functions
------

-- The primary definition of a permutation mathematically is as a bijection
-- Functions are often defined in terms of ordered pairs
-- Assumes that for elements $(a, b)$ and $(a', b')$ that $a = a'$ if and only if $b = b'$
-- Additionally assumes that if $(a, b)$ is an element then there are elements $(b, c)$ and $(d, a)$
-- Filter out fixed points so that they don't affect the equality derived from map equality
fromPairs :: Ord a => [(a, a)] -> Maybe (Permutation a)
fromPairs zs
  | isBijection = Just $ fromPairs' zs
  | otherwise = Nothing
  where
    (xs, ys) = unzip . map head . L.group $ L.sort zs
    (xs', ys') = (L.sort xs, L.sort ys)
    isBijection = xs' == ys' && all ((== 1) . length) (L.group xs')
    fromPairs' = P . M.fromList . filter (uncurry (/=))

toPairs :: Ord a => Permutation a -> [(a, a)]
toPairs (P g) = M.toList g

-- Assumes that each element defines a cycle
fromCycles :: Ord a => [[a]] -> Maybe (Permutation a)
fromCycles = fmap mconcat . mapM fromCycle
  where
    fromCycle [] = Just i
    fromCycle cs@(x : xs) = fromPairs $ zip cs $ xs ++ [x]

toCycles :: Ord a => Permutation a -> [[a]]
toCycles o@(P m) = toCycles' (M.keys m)
  where
    toCycles' [] = []
    toCycles' xs@(x : _) = let c = cycleOf x o in c : toCycles' (xs L.\\ c)

toFunction :: Ord a => Permutation a -> a -> a
toFunction (P g) x = M.findWithDefault x x g

------
-- Testing Instances
------

-- Permutations cannot be made an instance of QuickCheck.Arbitrary, since they are not aware of the
-- set they permute on the type-level
-- I.e. a value with type Permutation Int could be a permutation of any subset of Int

-- | Generate a random permutation of the given list. For use with "Test.QuickCheck".
permutationOf :: Ord a => [a] -> Q.Gen (Permutation a)
permutationOf xs = pp . zip xs <$> Q.shuffle xs

instance (Ord a, Q.Arbitrary a) => Q.Arbitrary (Permutation a) where
  arbitrary :: Q.Arbitrary a => Q.Gen (Permutation a)
  arbitrary = Q.arbitrary >>= permutationOf
