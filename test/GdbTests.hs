module GdbTests (gdbTests) where

import           Test.Tasty
import           Test.Tasty.Golden

import qualified Data.ByteString.Lazy    as LBS
import           Data.String
import qualified Data.Text.IO            as T
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           System.FilePath
import           Text.Pretty.Simple      (pShowNoColor)

import           Mello.GdbMI

gdbTests :: IO TestTree
gdbTests = testGroup "GDB" <$> sequenceA
  [ gdbMiParseTests
  ]

gdbMiParseTests :: IO TestTree
gdbMiParseTests = do
  miInputFiles <- findByExtension [".mi-output"] "./test/golden"
  pure $ testGroup "MI output parsing (golden)"
    [ goldenVsString
        (takeBaseName input)
        output
        (encodeUtf8 . pShowNoColor . parseGdbOutput <$> T.readFile input)
    | input <- miInputFiles
    , let output = replaceExtension input ".mi-parsed"
    ]

