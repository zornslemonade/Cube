import CubeTest (cubeTests)
import PermutationTest (permutationTests)
import Test.Tasty
import TupleTest (tupleTests)

-- Run all tests
fullTests :: TestTree
fullTests =
  testGroup
    "All Tests"
    [ permutationTests,
      tupleTests,
      cubeTests
    ]

main :: IO ()
main = defaultMain fullTests