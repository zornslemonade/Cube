module Permutation where

import qualified Data.List as L
import qualified Data.Map as M
import Modular
import qualified Test.Tasty.QuickCheck as Q

------
-- Permutations are bijections, thought of with a multiplicative group structure which acts from the left on the underlying set
------

-- Permutations will be stored as maps
newtype Permutation a = P (M.Map a a) deriving (Eq)

-- Construct a permutation from a list of cycles
-- This will be one of the primary ways of constructing permutations
-- This is the unsafe version of fromCycles
p :: Ord a => [[a]] -> Permutation a
p cs =
  case fromCycles cs of
    Just o -> o
    Nothing -> error "Not a permutation"

-- Construct a permutation from a list of pairs
-- This will be the other primary way of constructing permutations
-- This is the unsafe version of fromPairs
pp :: Ord a => [(a, a)] -> Permutation a
pp zs =
  case fromPairs zs of
    Just o -> o
    Nothing -> error "Not a permutation"

------
-- Instantiating type classes
------

-- Using the Num Class to (ab)use $(*)$ for multiplication
-- Also allows writing $1$ for the identity permutation
instance Ord a => Num (Permutation a) where
  o * q = pp [(x, o ?. q ?. x) | x <- support o ++ support q]
  fromInteger 1 = P M.empty
  _ + _ = error "(Permutation a).+: not applicable"
  negate _ = error "(Permutation a).negate: not applicable"
  abs _ = error "(Permutation a).abs: not applicable"
  signum _ = error "(Permutation a).signum: not applicable"

showInline :: (Ord a, Show a) => Permutation a -> String
showInline g
  | g == 1 = "1"
  | otherwise = concatMap showCycle $ toCycles g
  where
    showCycle xs = "(" ++ unwords (map show xs) ++ ")"

showMultiline :: (Ord a, Show a) => Permutation a -> String
showMultiline g
  | g == 1 = "1"
  | otherwise = L.intercalate "\n" (map showCycle (toCycles g))
  where
    showCycle xs = "(" ++ unwords (map show xs) ++ ")"

instance (Ord a, Show a) => Show (Permutation a) where
  show = showMultiline

------
-- Useful mathematical functions of permutations
------

-- Invert a permutation
inverseP :: Ord a => Permutation a -> Permutation a
inverseP (P g) = P $ M.fromList $ map (\(x, y) -> (y, x)) $ M.toList g

-- Use a notational trick to define $(^)$ for negative exponents
infixl 8 ^-

(^-) :: (Ord a, Integral b) => Permutation a -> b -> Permutation a
o ^- n = inverseP o ^ n

-- Point and set-wise applications of permuations
infixr 1 ?.

(?.) :: Ord a => Permutation a -> a -> a
o ?. x = toFunction o x

infixr 1 ?-

(?-) :: Ord a => Permutation a -> [a] -> [a]
o ?- xs = [o ?. x | x <- xs]

-- Conjugation of two permutations
infix 8 ?^?

(?^?) :: Ord a => Permutation a -> Permutation a -> Permutation a
o ?^? q = q ^- 1 * o * q

-- Commutator of two perumutations
infix 7 >?<

(>?<) :: Ord a => Permutation a -> Permutation a -> Permutation a
o >?< q = o ^- 1 * q ^- 1 * o * q

parity :: Ord a => Permutation a -> Mod2
parity o = M2 $ foldr ((+) . (+ 1) . toInteger . length) 0 (toCycles o)

sgn :: (Ord a, Integral b) => Permutation a -> b
sgn = ((-1) ^) . toIntegral . parity

orderE :: Ord a => Permutation a -> Int
orderE = L.foldl' lcm 1 . map length . toCycles

orderS :: Ord a => [Permutation a] -> Int
orderS = L.foldl' lcm 1 . map orderE

-- >>> o = p [[1,2,3],[4,5],[6,7,8,9]]
-- >>> cycleOf2 4 o
-- [4,5]

cycleOf :: Ord a => a -> Permutation a -> [a]
cycleOf x o = cycleThrough x
  where
    cycleThrough y = let y' = o ?. y in if y' == x then [y] else y : cycleThrough y'

-- The support of a permutation (the set of non-fixed points)
-- (Works for permutations created using the supplied constructors)
support :: Ord a => Permutation a -> [a]
support (P g) = M.keys g

-- Given z, decompose a permutation into a product of transpositions of the form (z x)
-- This can be done for any permutation and any z
transpositionDecomposition :: Ord a => a -> Permutation a -> [[a]]
transpositionDecomposition z o = concatMap cycleToTranspositions $ toCycles o
  where
    cycleToTranspositions (x : xs)
      | x == z = process xs
      | otherwise = [z, x] : process (x : xs)
    process [] = []
    process (x : xs) = process xs ++ [[z, x]]

-- Given z1, z2 decompose a permutation into a product of 3-cycles of the form (z1 z2 x), (z2 z1 x)
-- This can be done for any even permutation and any z1, z2
threeCycleDecomposition :: Ord a => a -> a -> Permutation a -> [[a]]
threeCycleDecomposition z1 z2 o = pairOff $ transpositionDecomposition z1 o
  where
    pairOff ([_, a] : [_, b] : xs)
      | a == b = pairOff xs
      | a == z2 = [z2, z1, b] : pairOff xs
      | b == z2 = [z1, z2, a] : pairOff xs
      | otherwise = [[z2, z1, b], [z2, z1, a], [z1, z2, b]] ++ pairOff xs
    pairOff _ = []

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
  | isPermutation = Just $ fromPairs' zs
  | otherwise = Nothing
  where
    (xs, ys) = unzip . map head . L.group $ L.sort zs
    (xs', ys') = (L.sort xs, L.sort ys)
    isPermutation = xs' == ys' && all ((== 1) . length) (L.group xs')
    fromPairs' = P . M.fromList . filter (uncurry (/=))

toPairs :: Ord a => Permutation a -> [(a, a)]
toPairs (P g) = M.toList g

-- Assumes that each element defines a cycle
fromCycles :: Ord a => [[a]] -> Maybe (Permutation a)
fromCycles = fmap product . mapM fromCycle
  where
    fromCycle [] = Just 1
    fromCycle cs@(x : xs) = fromPairs $ zip cs $ xs ++ [x]

-- | POOP
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

-- Generate a random permutation of the given list
permutationOf :: Ord a => [a] -> Q.Gen (Permutation a)
permutationOf xs = pp . zip xs <$> Q.shuffle xs