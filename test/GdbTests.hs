{-# OPTIONS_GHC
  -Wno-orphans -Wno-missing-deriving-strategies
  -Wno-name-shadowing -Wno-incomplete-uni-patterns
  -Wno-type-defaults -Wno-unused-top-binds
#-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
module GdbTests (gdbTests) where

import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.Golden.Advanced
import           Test.Tasty.HUnit

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import           Data.Either
import           Data.HashMap.Strict        as HM
import           Data.Maybe
import           Data.Text
import qualified Data.Text.IO               as T
import           Data.Text.Read
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.Process

import           Mello.Gdb.MI
import           Mello.Gdb.Session

-- These orphan instances are only used in golden tests
deriving instance Read MIMessage
deriving instance Read Value
deriving instance Read ResultRecord
deriving instance Read ResultClass
deriving instance Read AsyncRecord
deriving instance Read AsyncType
deriving instance Read StreamRecord
deriving instance Read StreamType

gdbTests :: IO TestTree
gdbTests = testGroup "GDB" <$> sequenceA
  [ gdbMiParseTests
  , gdbInteractionTests
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

runShell :: String -> IO (Text, ExitCode)
runShell cmd = do
    (_, Just stdout, _, phdl) <- createProcess $ (shell cmd){ std_out = CreatePipe }
    exitCode <- waitForProcess phdl
    out <- T.hGetContents stdout
    pure (out, exitCode)

-- GDB on Darwin/macOS SUCKS! WOW!
-- See https://sourceware.org/bugzilla/show_bug.cgi?id=24069
-- Therefore, on Darwin we only run tests that don't actually start any
-- child processes
onDarwin :: IO Bool
onDarwin = do
    (uname, _) <- runShell "uname"
    if (uname == "Darwin\n")
    then pure True
    else pure False

gdbInteractionTests :: IO TestTree
gdbInteractionTests = testGroup "GDB interaction tests"
    <$> ((checkGdbInstalled >> tests) <|> pure [])
  where
    checkGdbInstalled = do
        -- Check that GDB is installed
        (_, exitCode) <- runShell "gdb --version"
        guard $ exitCode == ExitSuccess

    tests = pure $
        [ parseMainAddress
        , asyncBkptAdd
        ]

-- Test whether we can successfully retrieve the address for a symbol.  We need
-- to pick up a binary which is not stripped. The easiest option is to just
-- pick our own binary.
parseMainAddress :: TestTree
parseMainAddress = testCase "parse_main_address" $ do
    myPath <- getExecutablePath
    let config = GdbConfig Nothing Nothing [myPath] []
    -- let config = GdbConfig Nothing (Just ("tmux", ["splitw", "-h"])) [myPath] []
    gdb <- spawnGdb config
    symbols <- sendCommand "-symbol-info-functions --include-nondebug --name main" gdb
    main <- sendCommand "-data-evaluate-expression (long)(&main)" gdb

    nondebug <- case (
        HM.lookup "symbols" >=> valueToTuple >=>
        HM.lookup "nondebug" >=> valueToList
        $ resultResults symbols
      ) of
        Just x -> pure x
        _      -> assertFailure "no symbols>nondebug field"
    let (String hexMainAddress):[] = catMaybes
            [ HM.lookup "address" hm |
            (_, Tuple hm) <- nondebug,
            HM.lookup "name" hm == Just (String "main") ]
    mainAddress1 <- case (hexadecimal hexMainAddress) of
        Left err     -> assertFailure $ "unable to parse hex address: " ++ err
        Right (x, _) -> pure x
    decMainAddress <- case (HM.lookup "value" >=> valueToString $ resultResults main) of
        Just x -> pure x
        _      -> assertFailure "no value returned"
    mainAddress2 <- case (decimal decMainAddress) of
        Left err     -> assertFailure $ "unable to parse value of (long)(&main): " ++ err
        Right (x, _) -> pure x
    mainAddress2 @?= mainAddress1

asyncBkptAdd :: TestTree
asyncBkptAdd = testCase "async_bkpt_add" $ do
    myPath <- getExecutablePath
    let config = GdbConfig Nothing Nothing [myPath] []
    -- let config = GdbConfig Nothing (Just ("tmux", ["splitw", "-h"])) [myPath] []
    gdb <- spawnGdb config
    mvar <- (newEmptyMVar :: IO (MVar (Either SomeException Word)))
    void $ flip (registerAsyncNotifyHandler "breakpoint-created") gdb $
      \record -> putMVar mvar <=< try $ do
        mainAddr <- case (
            HM.lookup "bkpt" >=> valueToTuple >=>
            HM.lookup "addr" >=> valueToString $
            asyncResults record
          ) of
            Just "<PENDING>" -> error "symbol 'main' not found"
            Just x           -> pure x
            _                -> error "no bkpt>addr field"
        case (hexadecimal mainAddr) of
            Left err     -> error $ "unable to parse hex address: " ++ err
            Right (x, _) -> pure x
    bkpt <- sendCommand "b main" gdb
    resultClass bkpt @?= Done
    main <- takeMVar mvar
    isRight main @? displayException (fromLeft undefined main :: SomeException)

