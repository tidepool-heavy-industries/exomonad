{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
-- | Smoke test for LSP client against real HLS
--
-- Run with: cabal run lsp-smoke-test
--
-- This test uses lsp-test which handles HLS spawning and LSP handshake.
--
module Main where

import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (hFlush, stdout)

import Language.LSP.Test
  ( Session, runSessionWithConfig, fullLatestClientCaps
  , openDoc, getHover, getDefinitions, request
  , defaultConfig, SessionConfig(..)
  )
import qualified Language.LSP.Protocol.Types as L
import qualified Language.LSP.Protocol.Message as L
import System.Directory (setCurrentDirectory)

-- | Flush stdout after each log
log' :: String -> IO ()
log' s = putStrLn s >> hFlush stdout

-- | Log inside Session monad
slog :: String -> Session ()
slog = liftIO . log'

-- | Test project directory (must exist with Test.hs)
testProjectDir :: FilePath
testProjectDir = "/tmp/lsp-test-project"

-- | Tidepool codebase directory for real LSP testing
tidepoolDir :: FilePath
tidepoolDir = "/private/tmp/tpw1"

-- | Session config with extended timeout and debug logging
testConfig :: SessionConfig
testConfig = defaultConfig
  { messageTimeout = 180  -- 3 minutes to allow HLS indexing
  , logStdErr = True      -- Show HLS stderr output
  , logMessages = True    -- Trace LSP messages
  }

main :: IO ()
main = do
  log' "=== LSP Smoke Test (lsp-test) ==="
  log' ""
  log' $ "[1] Starting HLS in " ++ testProjectDir ++ " (timeout: 180s)..."

  -- Change to test project directory so HLS uses correct cwd for project discovery
  -- (HLS uses cwd for hie.yaml lookup, not just rootUri)
  setCurrentDirectory testProjectDir

  -- Use wrapper script that includes --lsp flag
  result <- try @SomeException $ runSessionWithConfig testConfig "/tmp/hls-lsp-wrapper" fullLatestClientCaps testProjectDir $ do
    slog "OK - HLS initialized with correct rootUri"
    slog ""

    -- Open the document (REQUIRED before any requests)
    slog "[2] Opening document..."
    doc <- openDoc "Test.hs" "haskell"
    slog $ "OK - Opened: Test.hs"
    slog ""

    -- Test hover
    slog "[3] Testing hover..."
    testHoverDoc doc

    -- Test definition
    slog "[4] Testing definition..."
    testDefinitionDoc doc

    pure ()

  case result of
    Left e -> do
      log' $ "FAILED: " ++ show e
    Right () -> do
      log' ""
      log' "[5] Session closed cleanly"
      log' "=== Simple Test Complete ==="

  -- Now test against the real tidepool codebase
  log' ""
  log' "=== Real Codebase Test (workspaceSymbol) ==="
  log' $ "[6] Starting HLS in " ++ tidepoolDir ++ "..."

  setCurrentDirectory tidepoolDir

  realResult <- try @SomeException $ runSessionWithConfig testConfig "/tmp/hls-lsp-wrapper" fullLatestClientCaps tidepoolDir $ do
    slog "OK - HLS initialized on tidepool codebase"
    slog ""

    -- Test workspaceSymbol with a real symbol name
    slog "[7] Testing workspace/symbol with 'withLSPSession'..."
    testWorkspaceSymbol "withLSPSession"

    slog "[8] Testing workspace/symbol with 'LLMKind'..."
    testWorkspaceSymbol "LLMKind"

    pure ()

  case realResult of
    Left e -> do
      log' $ "FAILED: " ++ show e
    Right () -> do
      log' ""
      log' "[9] Session closed cleanly"
      log' "=== All Tests Complete ==="

-- | Test hover on a known position
testHoverDoc :: L.TextDocumentIdentifier -> Session ()
testHoverDoc doc = do
  let pos = L.Position 4 0  -- Line 5 (0-indexed), "add" function

  slog $ "  Doc: " ++ show doc._uri
  slog "  Position: line 5, char 0 (add function)"

  result <- getHover doc pos

  case result of
    Nothing -> slog "  Result: null (no hover info)"
    Just hoverResult -> do
      slog "  Result: Got hover info!"
      let content = extractContent hoverResult._contents
      slog $ "  Content: " ++ take 200 (T.unpack content) ++ "..."

-- | Test go-to-definition
testDefinitionDoc :: L.TextDocumentIdentifier -> Session ()
testDefinitionDoc doc = do
  let pos = L.Position 11 14  -- Line 12 (0-indexed), "theAnswer" in main

  slog $ "  Doc: " ++ show doc._uri
  slog "  Position: line 12, char 14 (theAnswer usage)"

  result <- getDefinitions doc pos

  case result of
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

-- | Test workspace/symbol query
testWorkspaceSymbol :: Text -> Session ()
testWorkspaceSymbol query = do
  slog $ "  Query: \"" ++ T.unpack query ++ "\""

  let params = L.WorkspaceSymbolParams
        { L._workDoneToken = Nothing
        , L._partialResultToken = Nothing
        , L._query = query
        }

  resp <- request L.SMethod_WorkspaceSymbol params

  case resp of
    L.TResponseMessage _ _ (Right result) -> do
      let symbols = extractSymbols result
      slog $ "  Result: Found " ++ show (length symbols) ++ " symbol(s)"
      mapM_ printSymbol (take 5 symbols)
    L.TResponseMessage _ _ (Left err) -> do
      slog $ "  Error: " ++ show err

extractSymbols :: [L.SymbolInformation] L.|? ([L.WorkspaceSymbol] L.|? L.Null) -> [Text]
extractSymbols result = case result of
  L.InL infos -> map (._name) infos
  L.InR wsOrNull -> case wsOrNull of
    L.InL wsSymbols -> map (._name) wsSymbols
    L.InR L.Null -> []

printSymbol :: Text -> Session ()
printSymbol name = slog $ "    - " ++ T.unpack name
