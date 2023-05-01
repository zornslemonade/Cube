{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module PermutationTest (permutationTests) where

import Algebra.Additive as Additive
import Algebra.Ring as Ring
import Algebra.ToInteger as ToInteger
import Data.Group
import NumericPrelude
import Permutable
import Permutation
import Test.Tasty
import Test.Tasty.QuickCheck
import Tuple

-- Properties of the basic operationsS
prop_associative :: Permutation Integer -> Permutation Integer -> Permutation Integer -> Bool
prop_associative g h k = (g ? h) ? k == g ? (h ? k)

prop_identity :: Permutation Integer -> Bool
prop_identity o = o ? i == o && i ? o == o

prop_inverse :: Permutation Integer -> Bool
prop_inverse o = invert o ? o == i && o ? invert o == i

prop_exponent :: Permutation Integer -> Int -> Bool
prop_exponent o n
  | n >= 0 = o ?^ n == foldr (?) i (replicate n o)
  | otherwise = o ?^ n == foldr (?) i (replicate (-n) (invert o))

prop_order :: Permutation Integer -> Bool
prop_order o = o ?^ order o == i

-- Tests for the basic operations
basicOperationTests :: TestTree
basicOperationTests =
  testGroup
    "Tests of the basic operations"
    [ testProperty "Associativity of ?" prop_associative,
      testProperty "The identity of ? is I" prop_identity,
      testProperty "Inverses are given by invert" prop_inverse,
      testProperty "Exponentiation is given by ?^" prop_exponent,
      testProperty "Exponentiation order cancellation" prop_order
    ]

-- Run all tests
permutationTests :: TestTree
permutationTests =
  testGroup
    "Permutation Tests"
    [ basicOperationTests
    ]
