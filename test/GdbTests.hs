{-# OPTIONS_GHC -Wno-orphans -Wno-missing-deriving-strategies #-}
{-# LANGUAGE StandaloneDeriving #-}
module GdbTests (gdbTests) where

import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.Golden.Advanced

import qualified Data.Text.IO               as T
import           System.FilePath

import           Mello.GdbMI

-- These orphan instances are only used in golden tests
deriving instance Read MIMessage
deriving instance Read Value
deriving instance Read ResultRecord
deriving instance Read ResultClass
deriving instance Read AsyncRecord
deriving instance Read AsyncType
deriving instance Read StreamRecord
deriving instance Read StreamType
deriving instance Eq MIMessage
deriving instance Eq Value
deriving instance Eq ResultRecord
deriving instance Eq ResultClass
deriving instance Eq AsyncRecord
deriving instance Eq AsyncType
deriving instance Eq StreamRecord
deriving instance Eq StreamType

gdbTests :: IO TestTree
gdbTests = testGroup "GDB" <$> sequenceA
  [ gdbMiParseTests
  ]

gdbMiParseTests :: IO TestTree
gdbMiParseTests = do
  miMessageFiles <- findByExtension [".mi-output"] "./test/golden"
  pure $ testGroup "MI output parsing (golden)"
    [ goldenTest
        (takeBaseName message)                   -- test name
        (read <$> readFile parsed)               -- golden (correct) value
        (parseGdbOutput <$> T.readFile message)  -- tested value
        (\golden tested -> pure $                -- comparison function
          if golden == tested
          then Nothing
          else Just "Messages differ"
        )
        (writeBinaryFile parsed . show)
    | message <- miMessageFiles
    , let parsed = replaceExtension message ".mi-parsed"
    ]

