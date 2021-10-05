{-# LANGUAGE RecordWildCards #-}
{- |
Copyright: (c) 2021 Tito Sacchi
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Tito Sacchi <tito.sakki@gmail.com>

This module exposes an interface to spawn GDB (the GNU Debugger) sessions and
talk to the debugger with a file descriptor.
-}

module Mello.Gdb.Session where

import           Control.Concurrent
import           Control.Monad
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HM
import           Data.IntMap.Strict    (IntMap)
import qualified Data.IntMap.Strict    as IM
import           Data.IORef
import           Data.Maybe
import           Data.Text             (Text)
import qualified Data.Text.IO          as T
import           Debug.Trace
import           System.IO
import           System.Mem.Weak
import           System.Posix.IO
import           System.Posix.Terminal
import           System.Posix.Types
import           System.Process

import           Mello.Gdb.MI

-- | Configuration for spawning a GDB session. There are two types of sessions:
--
-- * Console-interactive sessions provide a user-facing console interpreter in
-- another terminal emulator and talk to Mello with a PTY allocated in
-- 'spawnGdb'. Interactive sessions provide a @pwntools@'s @gdb.attach@-like
-- behavior.
--
-- * MI-only sessions can be used if the user doesn't need to interact manually
-- with the session. They are spawned as child processes and talk to Mello via
-- their stdin\/stdout pipes.
--
-- When you build a 'GdbConfig', you can specify the type of session you want
-- with the 'consoleHandler' field. 'Nothing' means that no user interaction is
-- required and a MI-only session will be opened. For an interactive session,
-- provide a command to summon an interactive terminal (e.g. @Just ("tmux",
-- ["splitw", "-h"])@ or @Just ("terminator", ["-e"])@).
--
-- Both type of sessions will necessarily quit when the Haskell runtime exits,
-- because file descriptors will be closed and GDB will receive EOF.
data GdbConfig = GdbConfig
  {
    -- | Path to the GDB binary (if it does not contain a slash, the @PATH@
    -- environment variable is searched). If 'Nothing', @"gdb"@ is assumed.
    gdbPath         :: !(Maybe FilePath),
    -- 'Nothing': spawn a MI-only session with the @-interpreter mi2@
    -- command-line flag.  GDB will talk with its standard streams. 'Just'
    -- holds the command-line (executable name + args) of an executable used to
    -- summon a GDB session (e.g.  @tmux split-window -v@).
    consoleHandler  :: !(Maybe (FilePath, [String])),
    -- | Command line arguments passed to GDB.
    args            :: ![String],
    -- | Commands to send GDB as soon as it is ready.
    startupCommands :: ![String]
  }

-- | This type represents an open GDB session.
--
-- __Note:__ this type should be treated like an opaque one. Use the top-level
-- functions in this module to construct 'GdbHandle's, submit commands and
-- register handlers. Record fields getters are exported only for debugging
-- purposes and may become internal in a future release.
data GdbHandle = GdbHandle
  {
    -- | Original configuration used to summon this instance. Could be useful
    -- to restart the debugger session.
    config               :: !GdbConfig,
    -- | 'ThreadId' of the Haskell thread that talks to the debugger instance and
    -- parses messages.
    managerThread        :: !(Weak ThreadId),
    -- | Process handle of the debugger session (if it was invoked directly
    -- without a console handler).
    process              :: !(Maybe ProcessHandle),
    -- | 'Nothing' means that the debugger is running in MI-only mode and talks
    -- via stdin\/out. 'Just' holds the master and slave file descriptors of
    -- the pseudo-terminal device that the MI interface is running on.
    pty                  :: !(Maybe (Fd, Fd)),
    -- | The read end of the pipe/pty we're using to talk to the debugger.
    -- Holds the same underlying FD of 'writeHandle' if we're using a PTY
    -- device.
    readHandle           :: !Handle,
    -- | The write end of the pipe/pty we're using to talk to the debugger.
    writeHandle          :: !Handle,
    -- | Next available token: will be used to mark the next command.  Note
    -- that there is no locking mechanism, so this should be modified with
    -- 'atomicModifyIORef'' __before__ pushing the 'MVar' to 'pendingCommands'.
    -- Otherwise, concurrent threads sending commands may overwrite each
    -- other's entry.
    nextToken            :: !(IORef Int),
    -- | This 'IORef' holds a map of command tokens to empty 'MVar's that are
    -- waiting to be filled with the corresponding results. When that happens,
    -- the corresponding entry should be removed from the map (at any time it
    -- should contain only empty 'MVar's).
    pendingCommands      :: !(IORef (IntMap (MVar ResultRecord))),
    -- | This 'IORef's hold a map of async command classes ('Text' values) to
    -- empty 'MVar's that are waiting to be filled with asynchronous messages
    -- from GDB. A thread should be waiting on each of these 'MVar's to process
    -- the corresponding message. Note that if one of the 'MVar's is full, the
    -- manager thread may block and no other messages could be delivered until
    -- the 'MVar' becomes free.
    asyncNotifyCallbacks :: !(IORef (HashMap Text (MVar AsyncRecord))),
    -- | This 'IORef' may hold a 'MVar' used to signal asynchronous target
    -- execution information (e.g. started, stopped, disappeared, ...). If the
    -- 'MVar' is full while the manager tries to deliver an asynchronous message,
    -- the manager may block and no other other messages could be delivered until
    -- the 'MVar' becomes free.
    asyncExecCallback    :: !(IORef (Maybe (MVar AsyncRecord)))
  }

-- | Open a GDB session with the specified configuration.
spawnGdb :: GdbConfig -> IO GdbHandle
spawnGdb config@GdbConfig{..} = do
    -- Allocate a pty to run the MI interface on if it is needed
    (pty, readHandle, writeHandle, process) <- case consoleHandler of
      Just (consoleCmd, consoleArgs) -> do
        pty@(master, _slave) <- openPseudoTerminal
        ptyName <- getSlaveTerminalName master

        -- Disable the ECHO terminal option, otherwise we will read
        -- our own input commands and the parser will die immediately
        terminalAttrs <- getTerminalAttributes master
        setTerminalAttributes master
          ((terminalAttrs `withoutMode` EnableEcho) `withoutMode` HangupOnClose)
          Immediately

        let newUiCommand = "new-ui mi2 " ++ ptyName
        -- (tmpPath, tmpHdl) <- mkstemp "MelloGdbInit"
        -- hPutStrLn tmpHdl newUiCommand
        -- hClose tmpHdl

        let gdbCmdLine = showCommandForUser (fromMaybe "gdb" gdbPath) ["-ex", newUiCommand]
        traceIO $ "Running gdb with cmdline: " ++ gdbCmdLine
        traceIO $ "On PTY: " ++ ptyName
        void $ createProcess $
            (proc consoleCmd (consoleArgs ++ [gdbCmdLine])) {
               std_in = NoStream,
               std_out = NoStream,
               std_err = NoStream,
               close_fds = True
            }

        master' <- fdToHandle master
        return (Just pty, master', master', Nothing)

      Nothing -> do
        (writeHandle, readHandle, _, processHandle) <- createProcess $
            (proc (fromMaybe "gdb" gdbPath) (["-interpreter", "mi2"] ++ args)) {
                std_in = CreatePipe,
                std_out = CreatePipe,
                std_err = NoStream,
                close_fds = True
            }
        return (Nothing, fromJust readHandle, fromJust writeHandle, Just processHandle)

    nextToken <- newIORef 0
    pendingCommands <- newIORef IM.empty
    asyncExecCallback <- newIORef Nothing
    asyncNotifyCallbacks <- newIORef HM.empty
    managerThread <- forkIO >=> mkWeakThreadId $
        manage pendingCommands asyncExecCallback asyncNotifyCallbacks readHandle
    return GdbHandle{..}

  where
    manage pendingCommands asyncExecCallback asyncNotifyCallbacks readHandle =
      forever $ do
        gdbLine <- T.hGetLine readHandle
        case (parseGdbOutputLine gdbLine) of
          -- Parsing failed
          Left errorMsg -> error errorMsg

          -- If the result has no token, don't do anything with it
          Right (Just (Result result@ResultRecord{..})) | Just token <- resultToken -> do
            -- Atomically get and remove the MVar from the queue
            waitingMVar <- atomicModifyIORef' pendingCommands $
                (,) <$> IM.delete token <*> IM.lookup token
            case waitingMVar of
              Nothing   -> return ()
              Just mvar -> putMVar mvar result

          Right (Just (Async async@AsyncRecord{..}) ) | asyncType == Exec -> do
            waitingMVar <- readIORef asyncExecCallback
            case waitingMVar of
              Nothing   -> return ()
              Just mvar -> putMVar mvar async

          Right (Just (Async async@AsyncRecord{..}) ) | asyncType == Notify -> do
            waitingMVar <- HM.lookup asyncClass <$> readIORef asyncNotifyCallbacks
            case waitingMVar of
              Nothing   -> return ()
              Just mvar -> putMVar mvar async

          _ -> return ()

-- | Send an MI command to the GDB session and return an empty 'MVar' that will
-- be filled with the result.
sendCommandAsync :: Text -> GdbHandle -> IO (MVar ResultRecord)
sendCommandAsync command gdb = do
    -- Get the next available token
    token <- atomicModifyIORef' (nextToken gdb) $ (,) <$> (+1) <*> id
    -- Allocate an MVar and put it in the pending queue
    mvar <- newEmptyMVar
    atomicModifyIORef' (pendingCommands gdb) $ (,) <$> IM.insert token mvar <*> const ()
    -- Send the command (token + command line) on the write end of the pipe
    hPutStr (writeHandle gdb) (show token)
    T.hPutStrLn (writeHandle gdb) command
    hFlush (writeHandle gdb)
    return mvar

-- | Send a MI command to the GDB session and wait for its result.
sendCommand :: Text -> GdbHandle -> IO ResultRecord
sendCommand command = sendCommandAsync command >=> takeMVar

-- | Register a handler for asynchronous \"exec\" messages (target @*started@,
-- running, disappeared...) and return the 'ThreadId' for the thread that will
-- receive the messages from the manager and run the handler.
--
-- Note that the handler function should be non-blocking because if the handler
-- thread is busy and an asynchronous message meant to be processed by this
-- handler arrives, the manager thread could block and no messages could be
-- processed until the handler thread is free again. You could get non-blocking
-- behavior by spawning a Haskell thread for each incoming message like this:
--
-- @
-- -- Your handler is processExecInfo :: AsyncRecord -> IO ()
-- registerAsyncHandler (processExecInfo >>= forkIO) gdbHandle
-- @
--
-- The handler thread will probably receive the
-- 'Control.Exception.BlockedIndefinitelyOnMVar' exception if the handler is
-- replaced with another one (with another call to this function) or if the GDB
-- session dies, but this is not guaranteed by the RTS and it is the user's
-- responsibility to ensure that any required cleanup is performed.
registerAsyncExecHandler
  :: (AsyncRecord -> IO ())    -- ^ Handler (callback function)
  -> GdbHandle                 -- ^ Handle to the GDB session
  -> IO (Weak ThreadId)
registerAsyncExecHandler handler gdb = do
    mvar <- newEmptyMVar
    atomicWriteIORef (asyncExecCallback gdb) (Just mvar)
    forkIO >=> mkWeakThreadId $
      forever $ takeMVar mvar >>= handler

-- | De-register the handler for asynchronous \"exec\" messages, if it exists.
-- The thread that passed messages to the handler will probably receive a
-- 'Control.Exception.BlockedIndefinitelyOnMVar' exception -- it should
-- probably be interrupted by the user before that happens.
deRegisterAsyncExecHandler :: GdbHandle -> IO ()
deRegisterAsyncExecHandler gdb =
  atomicWriteIORef (asyncExecCallback gdb) Nothing

-- | Register a handler for a specific asynchronous \"notify\" message class
-- (e.g. @=breakpoint-added,bkpt={...}@ has message class @breakpoint-added@)
-- and return the 'ThreadId' for the thread that will receive the messages from
-- the manager and run the handler.
--
-- The handler should be non-blocking. See documentation for
-- 'registerAsyncExecHandler' for concurrency caveats.
registerAsyncNotifyHandler
  :: Text                      -- ^ Async message class (e.g. @thread-group-added@)
  -> (AsyncRecord -> IO ())    -- ^ Handler (callback function)
  -> GdbHandle                 -- ^ Handle to the GDB session
  -> IO (Weak ThreadId)
registerAsyncNotifyHandler asyncClass handler gdb = do
    mvar <- newEmptyMVar
    atomicModifyIORef' (asyncNotifyCallbacks gdb) $ (,) <$> HM.insert asyncClass mvar <*> const ()
    forkIO >=> mkWeakThreadId $
      forever $ takeMVar mvar >>= handler

-- | De-register the handler for a specific asynchronous \"notify\" message class.
-- The thread that passed messages to the handler will probably receive a
-- 'Control.Exception.BlockedIndefinitelyOnMVar' exception -- it should
-- probably be interrupted by the user before that happens.
deRegisterAsyncNotifyHandler :: Text -> GdbHandle -> IO ()
deRegisterAsyncNotifyHandler asyncClass gdb =
  atomicModifyIORef' (asyncNotifyCallbacks gdb) $ (,) <$> HM.delete asyncClass <*> const ()

