{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
-- | Smoke test for LSP client against real HLS
--
-- Run with: cabal run lsp-smoke-test
--
-- This test runs all LSP requests inside a single session to avoid
-- the overhead of re-initializing HLS for each request.
--
module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (Handle, BufferMode(..), hFlush, hSetBuffering, stdout)
import System.Process (CreateProcess(..), StdStream(..), createProcess, proc, terminateProcess, cwd)

import Language.LSP.Client (runSessionWithHandles)
import Language.LSP.Client.Session (Session, initialize, request, openDoc)
import qualified Language.LSP.Protocol.Types as L
import qualified Language.LSP.Protocol.Message as L

-- | Flush stdout after each log
log' :: String -> IO ()
log' s = putStrLn s >> hFlush stdout

-- | Log inside Session monad
slog :: String -> Session ()
slog = liftIO . log'

main :: IO ()
main = do
  log' "=== LSP Smoke Test (lsp-client) ==="
  log' ""

  -- Start HLS in the project directory
  log' "[1] Starting HLS in /tmp/lsp-test-project..."
  (Just stdin', Just stdout', Just _stderr', ph) <- createProcess
    (proc "haskell-language-server-wrapper" ["--lsp"])
      { std_in = CreatePipe
      , std_out = CreatePipe
      , std_err = CreatePipe
      , cwd = Just "/tmp/lsp-test-project"
      }

  -- Set no buffering for LSP protocol
  hSetBuffering stdin' NoBuffering
  hSetBuffering stdout' NoBuffering

  log' "OK - HLS process started"
  log' ""

  -- Run all tests in a single session
  log' "[2] Running tests in single session..."
  result <- try $ runSessionWithHandles stdout' stdin' $ do
    -- Initialize once
    _ <- initialize Nothing
    slog "OK - Session initialized"
    slog ""

    -- Open the document (REQUIRED before any requests)
    slog "[2.5] Opening document..."
    doc <- openDoc "/tmp/lsp-test-project/Test.hs" "haskell"
    slog $ "OK - Opened: " ++ show doc._uri

    -- Wait for HLS to process the file (it needs to typecheck)
    slog "[2.6] Waiting 3s for HLS to typecheck..."
    liftIO $ threadDelay 3000000  -- 3 seconds
    slog "OK - Done waiting"
    slog ""

    -- Test hover
    slog "[3] Testing hover..."
    testHoverDoc doc

    -- Test definition
    slog "[4] Testing definition..."
    testDefinitionDoc doc

    pure ()

  -- Cleanup
  terminateProcess ph

  case result of
    Left (e :: SomeException) -> do
      log' $ "FAILED: " ++ show e
    Right () -> do
      log' ""
      log' "[5] Session closed cleanly"
      log' "=== Test Complete ==="

-- | Test hover on a known position
testHoverDoc :: L.TextDocumentIdentifier -> Session ()
testHoverDoc doc = do
  let pos = L.Position 4 0  -- Line 5 (0-indexed), "add" function

  slog $ "  Doc: " ++ show doc._uri
  slog "  Position: line 5, char 0 (add function)"

  resp <- request L.SMethod_TextDocumentHover $ L.HoverParams
    { L._textDocument = doc
    , L._position = pos
    , L._workDoneToken = Nothing
    }

  case resp._result of
    Left err -> slog $ "  Error: " ++ show err
    Right result -> case result of
      L.InR L.Null -> slog "  Result: null (no hover info)"
      L.InL hoverResult -> do
        slog "  Result: Got hover info!"
        let content = extractContent hoverResult._contents
        slog $ "  Content: " ++ take 200 (T.unpack content) ++ "..."

-- | Test go-to-definition
testDefinitionDoc :: L.TextDocumentIdentifier -> Session ()
testDefinitionDoc doc = do
  let pos = L.Position 11 14  -- Line 12 (0-indexed), "theAnswer" in main

  slog $ "  Doc: " ++ show doc._uri
  slog "  Position: line 12, char 14 (theAnswer usage)"

  resp <- request L.SMethod_TextDocumentDefinition $ L.DefinitionParams
    { L._textDocument = doc
    , L._position = pos
    , L._workDoneToken = Nothing
    , L._partialResultToken = Nothing
    }

  case resp._result of
    Left err -> slog $ "  Error: " ++ show err
    Right result -> case result of
      L.InR nested -> case nested of
        L.InR L.Null -> slog "  Result: null (no definition)"
        L.InL links -> do
          slog $ "  Result: Found " ++ show (length links) ++ " definition link(s)"
          mapM_ printDefLink links
      L.InL (L.Definition locOrLocs) -> case locOrLocs of
        L.InL loc -> do
          slog "  Result: Found 1 definition"
          printLoc loc
        L.InR locs -> do
          slog $ "  Result: Found " ++ show (length locs) ++ " definition(s)"
          mapM_ printLoc locs

printLoc :: L.Location -> Session ()
printLoc loc = do
  let L.Uri u = loc._uri
  slog $ "    " ++ T.unpack u ++ ":" ++ show loc._range._start._line

printDefLink :: L.DefinitionLink -> Session ()
printDefLink (L.DefinitionLink link) = do
  let L.Uri u = link._targetUri
  slog $ "    " ++ T.unpack u ++ ":" ++ show link._targetRange._start._line

extractContent :: L.MarkupContent L.|? (L.MarkedString L.|? [L.MarkedString]) -> Text
extractContent content = case content of
  L.InL mc -> mc._value
  L.InR msOrList -> case msOrList of
    L.InL ms -> markedToText ms
    L.InR msList -> T.intercalate "\n" (map markedToText msList)

markedToText :: L.MarkedString -> Text
markedToText (L.MarkedString inner) = case inner of
  L.InL t -> t
  L.InR mswl -> mswl._value
