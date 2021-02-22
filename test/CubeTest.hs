module CubeTest (cubeTests) where

import Cube
import Permutable
import Permutation (p)
import Data.Group
import Test.Tasty
import Test.Tasty.QuickCheck

i = turnToConfig I

-- Example configurations for testing
flippedEdges :: CubeConfiguration
flippedEdges = Cube (1, 1, 1, 0, T12 (1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 0)

flippedEdgesIllegal :: CubeConfiguration
flippedEdgesIllegal = Cube (1, 1, 1, 0, T12 (1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0), 0)

swappedVertices :: CubeConfiguration
swappedVertices = Cube (1, 1, p [[1, 2, 3]], 0, 0, 0)

swappedVerticesIllegal :: CubeConfiguration
swappedVerticesIllegal = Cube (1, 1, p [[1, 2]], 0, 0, 0)

similarityExample1 :: CubeConfiguration
similarityExample1 = Cube (p [[1, 3, 5, 4, 2]], p [[1, 9, 7, 5, 2, 8, 10, 12, 3, 4, 11, 6]], p [[1, 5, 3], [4, 6, 8, 7]], T6 (2, 0, 1, 1, 0, 0), T12 (0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0), T8 (1, 1, 0, 0, 1, 1, 1, 2))

similarityExample2 :: CubeConfiguration
similarityExample2 = Cube (p [[1, 3, 5, 4, 2]], p [[1, 2, 3]], p [[]], T6 (1, 0, 0, 0, 0, 0), T12 (1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0), T8 (1, 1, 2, 0, 0, 0, 0, 0))

-- Properties of the basic operationsS
prop_associative :: CubeConfiguration -> CubeConfiguration -> CubeConfiguration -> Bool
prop_associative g h k = (g # h) # k == g # (h # k)

prop_identity :: CubeConfiguration -> Bool
prop_identity g = g # i == g && i # g == g

prop_inverse :: CubeConfiguration -> Bool
prop_inverse g = invert g # g == i && g # invert g == i

prop_exponent :: CubeConfiguration -> Int -> Bool
prop_exponent g n
  | n >= 0 = g #^ n == foldr (#) i (replicate n g)
  | otherwise = g #^ n == foldr (#) i (replicate (abs n) (invert g))

prop_order :: CubeConfiguration -> Bool
prop_order g = g #^ order g == i

-- Tests for the basic operations
basicOperationTests :: TestTree
basicOperationTests =
  testGroup
    "Tests of the basic operations"
    [ testProperty "Associativity of #" prop_associative,
      testProperty "The identity of # is I" prop_identity,
      testProperty "Inverses are given by invert" prop_inverse,
      testProperty "Exponentiation is given by #^" prop_exponent,
      testProperty "Exponentiation order cancellation" prop_order
    ]
-- Properties of the basic turns

prop_order_turn :: Turn -> Bool
prop_order_turn m =
  case m of
    I -> (== 1) . order . turnToConfig $ m
    _ -> (== 4) . order . turnToConfig $ m

prop_inverse_turn :: Turn -> Bool
prop_inverse_turn m = turnToConfig m # turnToConfig (invertTurn m) == i && turnToConfig (invertTurn m) # turnToConfig m == i

prop_inverse_turns :: [Turn] -> Bool
prop_inverse_turns m = turnsToConfig m # turnsToConfig (invertTurns m) == i && turnsToConfig (invertTurns m) # turnsToConfig m == i

prop_legal_turn :: Turn -> Bool
prop_legal_turn = isLegal . turnToConfig

prop_legal_turns :: [Turn] -> Bool
prop_legal_turns = isLegal . turnsToConfig

-- Tests for the basic turns
basicTurnTests :: TestTree
basicTurnTests =
  testGroup
    "Tests of the basic turns"
    [ testProperty "Basic turns have order 4" prop_order_turn,
      testProperty "Basic turns are inverted correctly" prop_inverse_turn,
      testProperty "Turn sequences are inverted correctly" prop_inverse_turns,
      testProperty "Basic turns yield legal configurations" prop_legal_turn,
      testProperty "Turn sequences yield legal configurations" prop_legal_turns
    ]

-- Properties of the similarity and legality relations
prop_legality_example1 :: Bool
prop_legality_example1 = isLegal flippedEdges

prop_illegality_example1 :: Bool
prop_illegality_example1 = not $ isLegal flippedEdgesIllegal

prop_legality_example2 :: Bool
prop_legality_example2 = isLegal swappedVertices

prop_illegality_example2 :: Bool
prop_illegality_example2 = not $ isLegal swappedVerticesIllegal

prop_reflexivity :: CubeConfiguration -> Bool
prop_reflexivity g = g `isSimilarTo` g

prop_similarity :: CubeConfiguration -> [Turn] -> Bool
prop_similarity g ms = let g' = g # turnsToConfig ms in g `isSimilarTo` g' && g' `isSimilarTo` g

prop_similarity_example :: Bool
prop_similarity_example = similarityExample1 `isSimilarTo` similarityExample2 && similarityExample2 `isSimilarTo` similarityExample1

-- Tests for the similarity and legality relations

relationTests :: TestTree
relationTests =
  testGroup
    "Tests of the similarity and legality relations"
    [ testProperty "Legality of two flipped edges" prop_legality_example1,
      testProperty "Legality of three swapped vertices" prop_legality_example2,
      testProperty "Illegality of three flipped edges" prop_illegality_example1,
      testProperty "Illegality of two swapped corners" prop_illegality_example2,
      testProperty "Configurations are self-similar" prop_reflexivity,
      testProperty "Similarity is unaffected by turn sequences" prop_similarity,
      testProperty "Similarity example" prop_similarity_example
    ]

-- Properties of the configuration solution process
prop_generation :: [Turn] -> Bool
prop_generation ms =
  let g = turnsToConfig ms
   in case Cube.generate g of
        Just ms' -> turnsToConfig ms' == g
        Nothing -> False

prop_solution :: [Turn] -> Bool
prop_solution ms =
  let g = turnsToConfig ms
   in case solve g of
        Just ms' -> let g' = turnsToConfig ms' in g # g' == i && g' # g == i
        Nothing -> False

prop_solution_example1 :: Bool
prop_solution_example1 = case solve flippedEdges of
  Just ms -> let g = turnsToConfig ms in g # flippedEdges == i && flippedEdges # g == i
  Nothing -> False

prop_solution_example2 :: Bool
prop_solution_example2 = case solve swappedVertices of
  Just ms -> let g = turnsToConfig ms in g # swappedVertices == i && swappedVertices # g == i
  Nothing -> False

prop_no_solution_example1 :: Bool
prop_no_solution_example1 = case solve swappedVerticesIllegal of
  Just _ -> False
  Nothing -> True

prop_no_solution_example2 :: Bool
prop_no_solution_example2 = case solve swappedVerticesIllegal of
  Just _ -> False
  Nothing -> True

-- Tests for the configuration solution process
solutionTests :: TestTree
solutionTests =
  testGroup
    "Tests of the solution process"
    [ testProperty "Configurations generated by turn sequences are solved correctly" prop_solution,
      testProperty "Two flipped edges are solved correctly" prop_solution_example1,
      testProperty "Three swapped corners are solved correctly" prop_solution_example2,
      testProperty "Three flipped edges are not solved" prop_no_solution_example1,
      testProperty "Two swapped corners are not solved" prop_no_solution_example2
    ]

-- Run all tests
cubeTests :: TestTree
cubeTests =
  testGroup
    "Cube Tests"
    [ basicOperationTests,
      basicTurnTests,
      relationTests,
      solutionTests
    ]
