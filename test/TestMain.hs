import           Test.Tasty

import           GdbTests

main = defaultMain =<< tests

tests :: IO TestTree
tests = testGroup "Tests" <$> sequenceA
  [ gdbTests
  ]

