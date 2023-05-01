{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TupleTest (tupleTests) where

import Control.Applicative ((<$>))
import Data.Group
import NumericPrelude
import Permutable
import Permutation
import Test.Tasty
import Test.Tasty.QuickCheck
import Tuple

-- Properties of the basic operations on sextuples
newtype Shuffle6 = Six (Permutation Integer) deriving (Eq)

instance Show Shuffle6 where
  show :: Shuffle6 -> String
  show (Six o) = show o

instance Arbitrary Shuffle6 where
  arbitrary :: Gen Shuffle6
  arbitrary = Six <$> permutationOf [1 .. 6]

prop_associative_six :: Shuffle6 -> Shuffle6 -> Tuple6 Integer -> Bool
prop_associative_six (Six o) (Six r) x = o ?* (r ?* x) == (o ? r) ?* x

prop_identity_six :: Tuple6 Integer -> Bool
prop_identity_six x = i ?* x == x && x *? i == x

prop_inverse_six :: Shuffle6 -> Tuple6 Integer -> Bool
prop_inverse_six (Six o) x = invert o ?* x == x *? o

prop_index_six :: Shuffle6 -> Tuple6 Integer -> Bool
prop_index_six (Six o) x = and [(o ?* x) *! n == x *! (invert o ?. n) | n <- [1 .. 6]]

-- Tests for the basic operations
sextupleTests :: TestTree
sextupleTests =
  testGroup
    "Tests of the basic operations on sextuples"
    [ testProperty "Associativity of ?* action" prop_associative_six,
      testProperty "The identity of ?* is I" prop_identity_six,
      testProperty "The left action is the inverse of the right action" prop_inverse_six,
      testProperty "The ?* action acts on indices" prop_index_six
    ]

-- Properties of the basic operations on octtuples
newtype Shuffle8 = Eight (Permutation Integer) deriving (Eq)

instance Show Shuffle8 where
  show :: Shuffle8 -> String
  show (Eight o) = show o

instance Arbitrary Shuffle8 where
  arbitrary :: Gen Shuffle8
  arbitrary = Eight <$> permutationOf [1 .. 8]

prop_associative_eight :: Shuffle8 -> Shuffle8 -> Tuple8 Integer -> Bool
prop_associative_eight (Eight o) (Eight r) x = o ?* (r ?* x) == (o ? r) ?* x

prop_identity_eight :: Tuple8 Integer -> Bool
prop_identity_eight x = i ?* x == x && x *? i == x

prop_inverse_eight :: Shuffle8 -> Tuple8 Integer -> Bool
prop_inverse_eight (Eight o) x = invert o ?* x == x *? o

prop_index_eight :: Shuffle8 -> Tuple8 Integer -> Bool
prop_index_eight (Eight o) x = and [(o ?* x) *! n == x *! (invert o ?. n) | n <- [1 .. 8]]

-- Tests for the basic operations
octupleTests :: TestTree
octupleTests =
  testGroup
    "Tests of the basic operations on octuples"
    [ testProperty "Associativity of ?* action" prop_associative_eight,
      testProperty "The identity of ?* is I" prop_identity_eight,
      testProperty "The left action is the inverse of the right action" prop_inverse_eight,
      testProperty "The ?* action acts on indices" prop_index_eight
    ]

-- Properties of the basic operations on duodecuples
newtype Shuffle12 = Twelve (Permutation Integer) deriving (Eq)

instance Show Shuffle12 where
  show :: Shuffle12 -> String
  show (Twelve o) = show o

instance Arbitrary Shuffle12 where
  arbitrary :: Gen Shuffle12
  arbitrary = Twelve <$> permutationOf [1 .. 12]

prop_associative_twelve :: Shuffle12 -> Shuffle12 -> Tuple12 Integer -> Bool
prop_associative_twelve (Twelve o) (Twelve r) x = o ?* (r ?* x) == (o ? r) ?* x

prop_identity_twelve :: Tuple12 Integer -> Bool
prop_identity_twelve x = i ?* x == x && x *? i == x

prop_inverse_twelve :: Shuffle12 -> Tuple12 Integer -> Bool
prop_inverse_twelve (Twelve o) x = invert o ?* x == x *? o

prop_index_twelve :: Shuffle12 -> Tuple12 Integer -> Bool
prop_index_twelve (Twelve o) x = and [(o ?* x) *! n == x *! (invert o ?. n) | n <- [1 .. 12]]

-- Tests for the basic operations
duodecupleTests :: TestTree
duodecupleTests =
  testGroup
    "Tests of the basic operations on duodecuples"
    [ testProperty "Associativity of ?* action" prop_associative_twelve,
      testProperty "The identity of ?* is I" prop_identity_twelve,
      testProperty "The left action is the inverse of the right action" prop_inverse_twelve,
      testProperty "The ?* action acts on indices" prop_index_twelve
    ]

-- Run all tests
tupleTests :: TestTree
tupleTests =
  testGroup
    "Tuple Tests"
    [ sextupleTests,
      octupleTests,
      duodecupleTests
    ]
