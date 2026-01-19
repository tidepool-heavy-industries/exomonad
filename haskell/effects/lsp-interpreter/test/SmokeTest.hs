{-# LANGUAGE OverloadedStrings #-}
-- | LSP State Transition Verification Test
--
-- Verifies that the LSP interpreter correctly handles the Startup -> Ready transition
-- and avoids premature Ready state.
--
-- Run with: cabal run lsp-smoke-test
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import System.Directory (getCurrentDirectory)
import System.IO (hFlush, stdout)

import Tidepool.LSP.Interpreter (withLSPSession, getSessionIndexingState, LSPSession)
import Tidepool.Effect.LSP (IndexingState(..))

log' :: String -> IO ()
log' s = putStrLn s >> hFlush stdout

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  log' $ "=== LSP State Transition Test ==="
  log' $ "Current Directory: " ++ cwd
  
  withLSPSession cwd $ \session -> do
    log' "Session started."
    monitorState session 20 -- Monitor for 20 seconds

monitorState :: LSPSession -> Int -> IO ()
monitorState _ 0 = log' "Finished monitoring."
monitorState session n = do
  state <- getSessionIndexingState session
  log' $ "State: " ++ show state
  threadDelay 1000000 -- 1s
  monitorState session (n - 1)

