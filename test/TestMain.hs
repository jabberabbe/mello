import Mello

import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testCase "Fake" $ True @?= True
  ]

