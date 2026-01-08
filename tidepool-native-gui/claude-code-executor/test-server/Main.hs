-- | Test server for integration testing control socket.
--
-- Starts a control socket server with default (allow-all) callbacks,
-- prints the socket path to stdout, then blocks until stdin closes.
--
-- Usage from Rust:
--   1. Spawn this process
--   2. Read socket path from stdout
--   3. Connect and send test messages
--   4. Close stdin to terminate
module Main where

import System.IO (hFlush, stdout, hGetLine, stdin, hSetBuffering, BufferMode(..), hIsEOF)

import Tidepool.ClaudeCode.ControlSocket (withControlSocket)
import Tidepool.ClaudeCode.Hooks (HookCallbacks(..), defaultHookCallbacks, HookDecision(..))


main :: IO ()
main = do
  -- Line buffering for reliable pipe communication
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering

  -- Use custom callbacks that log to stderr for debugging
  let callbacks = defaultHookCallbacks
        { hcOnPreToolUse = \toolName _input -> do
            -- Return allow for most tools, deny for "test_deny"
            if toolName == "test_deny"
              then pure $ HookDeny "Denied by test server"
              else pure HookAllow
        , hcOnStop = pure $ HookBlock "Test server blocking stop"
        }

  withControlSocket callbacks $ \socketPath -> do
    -- Print socket path so client can find us
    putStrLn socketPath
    hFlush stdout

    -- Block until stdin closes (client finished testing)
    waitForEOF


-- | Wait for stdin to close.
waitForEOF :: IO ()
waitForEOF = do
  eof <- hIsEOF stdin
  if eof
    then pure ()
    else do
      -- Try to read, will block or get EOF
      _ <- hGetLine stdin
      waitForEOF
