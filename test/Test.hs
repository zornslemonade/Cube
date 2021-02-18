import CubeTest
import Test.Tasty

-- Run all tests
fullTests :: TestTree
fullTests =
  testGroup
    "All Tests"
    [ cubeTests
    ]

main :: IO ()
main = defaultMain fullTests