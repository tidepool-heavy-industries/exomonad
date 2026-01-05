-- | Core analysis logic using LSP, structured as a v2 graph handler.
--
-- Queries a language server for symbol information at a given position,
-- including hover info, definition location, and all references.
--
-- == Using lspmux for persistent HLS
--
-- By default, this spawns a fresh HLS process per invocation (~10s cold start).
-- For faster repeated queries, use lspmux to keep HLS warm:
--
-- @
-- 1. Install: cargo install lspmux
-- 2. Start server: lspmux server &
-- 3. Either:
--    - Set env: export IMPACT_LSP_CMD="lspmux client --server-path haskell-language-server-wrapper"
--    - Or use flag: impact-analysis --lsp-cmd "lspmux client --server-path haskell-language-server-wrapper"
-- @
--
module ImpactAnalysis.Analyze
  ( -- * Graph Execution
    runImpactGraph
    -- * Graph Components (for testing)
  , impactHandlers
  , analyzeHandler
    -- * Utilities
  , findProjectRoot
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, tryPutMVar, takeMVar)
import Control.Exception (bracket, try, SomeException)
import Control.Monad (void, when)
import Control.Monad.Freer (Eff, LastMember, sendM, runM)
import Control.Monad.IO.Class (liftIO)
import Data.List (isSuffixOf, sortBy)
import Data.Ord (comparing, Down(..))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import System.Directory (makeAbsolute, doesFileExist, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO (Handle, BufferMode(..), hSetBuffering)
import System.Process (CreateProcess(..), StdStream(..), ProcessHandle, createProcess, proc, terminateProcess, cwd)
import System.Timeout (timeout)

import Language.LSP.Client (runSessionWithHandles)
import Language.LSP.Client.Session (Session, initialize, request, openDoc, receiveNotification)
import qualified Language.LSP.Protocol.Types as L
import qualified Language.LSP.Protocol.Message as L

import Tidepool.Graph.Generic (AsHandler)
import Tidepool.Graph.Types (Exit)
import Tidepool.Graph.Goto (GotoChoice, gotoExit, To)
import Tidepool.Graph.Execute (runGraph)

import ImpactAnalysis.Types
import ImpactAnalysis.Graph


-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH HANDLERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Graph handlers - IO is the base effect for LSP operations.
impactHandlers :: ImpactGraph (AsHandler '[IO])
impactHandlers = ImpactGraph
  { igEntry = Proxy
  , igAnalyze = analyzeHandler
  , igExit = Proxy
  }

-- | LogicNode handler - lifts LSP analysis into effect stack.
analyzeHandler
  :: LastMember IO effs
  => ImpactInput
  -> Eff effs (GotoChoice '[To Exit ImpactOutput])
analyzeHandler input = do
  result <- sendM $ doLSPAnalysis input
  case result of
    Left err -> do
      -- On error, return empty output with error in symbol name
      let errorOutput = ImpactOutput
            { symbol = Just $ SymbolInfo ("Error: " <> err) ""
            , definedAt = []
            , references = []
            , stats = ImpactStats 0 0 []
            }
      pure $ gotoExit errorOutput
    Right output ->
      pure $ gotoExit output


-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH EXECUTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Run the complete graph: Entry → Analyze → Exit
runImpactGraph :: ImpactInput -> IO ImpactOutput
runImpactGraph input = runM $ runGraph impactHandlers input


-- ════════════════════════════════════════════════════════════════════════════
-- CORE LSP ANALYSIS (unchanged from IO version)
-- ════════════════════════════════════════════════════════════════════════════

-- | Run full impact analysis for a symbol at the given position.
doLSPAnalysis :: ImpactInput -> IO (Either Text ImpactOutput)
doLSPAnalysis input = do
  absPath <- makeAbsolute (file input)
  mProjectRoot <- findProjectRoot absPath

  case mProjectRoot of
    Nothing -> pure $ Left $ "Could not find project root for: " <> T.pack absPath
    Just projectRoot -> do
      result <- try $ withHLSSession (lspCmd input) projectRoot $ \hls ->
        runSessionWithHandles hls.hlsStdout hls.hlsStdin $ do
          _ <- initialize Nothing

          -- Set up MVar to signal when HLS is ready
          hlsReady <- liftIO newEmptyMVar
          let targetUri = L.filePathToUri absPath

          -- HLS sends progress notifications while indexing/typechecking.
          -- When progress ends, HLS is ready for queries.
          receiveNotification L.SMethod_Progress $ \_notif ->
            void $ tryPutMVar hlsReady ()

          -- Also listen for diagnostics as a fallback signal
          receiveNotification L.SMethod_TextDocumentPublishDiagnostics $ \notif ->
            when (notif._params._uri == targetUri) $
              void $ tryPutMVar hlsReady ()

          -- Open document (REQUIRED before any requests)
          doc <- openDoc absPath "haskell"

          -- Wait for HLS to signal readiness (progress or diagnostics)
          -- with a 10s timeout, then small delay to ensure indexing completes
          liftIO $ do
            void $ timeout 10000000 $ takeMVar hlsReady
            threadDelay 500000  -- 500ms buffer for HLS to finish

          -- Convert 1-indexed user input to 0-indexed LSP positions
          let pos = L.Position
                { L._line = fromIntegral (line input - 1)
                , L._character = fromIntegral (col input - 1)
                }

          -- Query all LSP endpoints
          hoverResult <- queryHover doc pos
          defResult <- queryDefinition doc pos
          refResult <- queryReferences doc pos

          pure $ buildOutput hoverResult defResult refResult

      case result of
        Left (e :: SomeException) ->
          pure $ Left $ "LSP error: " <> T.pack (show e)
        Right output ->
          pure $ Right output


-- | Find the project root by looking for cabal/stack/hie files.
findProjectRoot :: FilePath -> IO (Maybe FilePath)
findProjectRoot path = go (takeDirectory path)
  where
    go dir
      | takeDirectory dir == dir = pure Nothing  -- Reached filesystem root (cross-platform)
      | otherwise = do
          hasCabalProject <- doesFileExist (dir </> "cabal.project")
          hasStack <- doesFileExist (dir </> "stack.yaml")
          hasHie   <- doesFileExist (dir </> "hie.yaml")
          -- Also check for any .cabal file in the directory
          hasCabalFile <- hasCabalFileIn dir
          if hasCabalProject || hasStack || hasHie || hasCabalFile
            then pure (Just dir)
            else go (takeDirectory dir)

    hasCabalFileIn dir = do
      files <- listDirectory dir
      pure $ any (".cabal" `isSuffixOf`) files


-- ════════════════════════════════════════════════════════════════════════════
-- HLS SESSION MANAGEMENT
-- ════════════════════════════════════════════════════════════════════════════

data HLSSession = HLSSession
  { hlsStdin   :: Handle
  , hlsStdout  :: Handle
  , hlsProcess :: ProcessHandle
  }

-- | Get LSP server command. Priority: CLI flag > env var > default.
getLSPCommand :: Maybe String -> IO (String, [String])
getLSPCommand mCliCmd = do
  mEnvCmd <- lookupEnv "IMPACT_LSP_CMD"
  let cmd = mCliCmd <|> mEnvCmd
  pure $ case cmd of
    Just cmdStr -> parseCommand cmdStr
    Nothing -> ("haskell-language-server-wrapper", ["--lsp"])

-- | Parse a command string into executable and arguments.
-- Handles shell-style quoting (single/double quotes, backslash escapes).
parseCommand :: String -> (String, [String])
parseCommand s = case shellWords s of
  [] -> ("haskell-language-server-wrapper", ["--lsp"])
  (x:xs) -> (x, xs)

-- | Quote parsing mode for shell-style word splitting.
data ShellMode = ShellUnquoted | ShellInSingle | ShellInDouble

-- | Simple shell-style word splitter supporting:
--   * Unquoted words separated by whitespace
--   * Single quotes: 'literal text' (no escapes)
--   * Double quotes: "text with \" and \\ escapes"
--   * Backslash escapes in unquoted context
shellWords :: String -> [String]
shellWords = go [] [] ShellUnquoted
  where
    go :: [String] -> String -> ShellMode -> String -> [String]
    go acc cur _ [] = finishToken acc cur
    go acc cur mode (c:cs) = case mode of
      ShellUnquoted -> case c of
        ' '  -> go (finishToken acc cur) [] ShellUnquoted cs
        '\t' -> go (finishToken acc cur) [] ShellUnquoted cs
        '\'' -> go acc cur ShellInSingle cs
        '"'  -> go acc cur ShellInDouble cs
        '\\' -> case cs of
          (e:rest) -> go acc (e:cur) ShellUnquoted rest
          []       -> go acc ('\\':cur) ShellUnquoted []
        _    -> go acc (c:cur) ShellUnquoted cs
      ShellInSingle -> case c of
        '\'' -> go acc cur ShellUnquoted cs
        _    -> go acc (c:cur) ShellInSingle cs
      ShellInDouble -> case c of
        '"'  -> go acc cur ShellUnquoted cs
        '\\' -> case cs of
          (e:rest) | e `elem` ("\"\\$`" :: String) -> go acc (e:cur) ShellInDouble rest
          (e:rest) -> go acc (e:'\\':cur) ShellInDouble rest
          []       -> go acc ('\\':cur) ShellInDouble []
        _    -> go acc (c:cur) ShellInDouble cs

    finishToken :: [String] -> String -> [String]
    finishToken acc [] = acc
    finishToken acc cur = acc ++ [reverse cur]

withHLSSession :: Maybe String -> FilePath -> (HLSSession -> IO a) -> IO a
withHLSSession mLspCmd projectRoot = bracket acquire release
  where
    acquire = do
      (cmd, args) <- getLSPCommand mLspCmd
      -- createProcess with CreatePipe always returns Just handles,
      -- but we handle the Nothing case explicitly for safety
      (mStdin, mStdout, mStderr, ph) <- createProcess
        (proc cmd args)
          { std_in = CreatePipe
          , std_out = CreatePipe
          , std_err = CreatePipe
          , cwd = Just projectRoot
          }
      case (mStdin, mStdout, mStderr) of
        (Just stdin', Just stdout', Just _stderr') -> do
          hSetBuffering stdin' NoBuffering
          hSetBuffering stdout' NoBuffering
          pure $ HLSSession stdin' stdout' ph
        _ -> fail "Failed to create HLS process: missing standard IO handles"

    release hls = terminateProcess hls.hlsProcess


-- ════════════════════════════════════════════════════════════════════════════
-- LSP QUERIES
-- ════════════════════════════════════════════════════════════════════════════

queryHover :: L.TextDocumentIdentifier -> L.Position -> Session (Maybe (Text, Text))
queryHover doc pos = do
  resp <- request L.SMethod_TextDocumentHover $ L.HoverParams
    { L._textDocument = doc
    , L._position = pos
    , L._workDoneToken = Nothing
    }
  pure $ case resp._result of
    Left _err -> Nothing
    Right result -> case result of
      L.InR L.Null -> Nothing
      L.InL hover -> Just (extractHoverInfo hover)

queryDefinition :: L.TextDocumentIdentifier -> L.Position -> Session [L.Location]
queryDefinition doc pos = do
  resp <- request L.SMethod_TextDocumentDefinition $ L.DefinitionParams
    { L._textDocument = doc
    , L._position = pos
    , L._workDoneToken = Nothing
    , L._partialResultToken = Nothing
    }
  pure $ case resp._result of
    Left _err -> []
    Right result -> extractDefinitions result

queryReferences :: L.TextDocumentIdentifier -> L.Position -> Session [L.Location]
queryReferences doc pos = do
  resp <- request L.SMethod_TextDocumentReferences $ L.ReferenceParams
    { L._textDocument = doc
    , L._position = pos
    , L._workDoneToken = Nothing
    , L._partialResultToken = Nothing
    , L._context = L.ReferenceContext { L._includeDeclaration = False }
    }
  pure $ case resp._result of
    Left _err -> []
    Right result -> case result of
      L.InR L.Null -> []
      L.InL locs -> locs


-- ════════════════════════════════════════════════════════════════════════════
-- RESULT EXTRACTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Extract symbol name and type signature from hover result.
extractHoverInfo :: L.Hover -> (Text, Text)
extractHoverInfo hover =
  let content = extractMarkupContent hover._contents
      -- Parse the content to extract name and type
      -- HLS typically returns something like:
      -- ```haskell
      -- foo :: Int -> Int
      -- ```
      -- or just:
      -- foo :: Int -> Int
      lines' = T.lines content
      -- Filter out markdown code fences
      cleaned = filter (not . isCodeFence) lines'
      sig = T.strip $ T.unlines cleaned
      -- Extract name from type signature (before ::)
      name = case T.breakOn "::" sig of
        (n, rest) | not (T.null rest) -> T.strip n
        _ -> sig  -- No ::, just use whole thing as name
  in (name, sig)
  where
    isCodeFence t = "```" `T.isPrefixOf` T.strip t

extractMarkupContent :: L.MarkupContent L.|? (L.MarkedString L.|? [L.MarkedString]) -> Text
extractMarkupContent content = case content of
  L.InL mc -> mc._value
  L.InR msOrList -> case msOrList of
    L.InL ms -> markedStringToText ms
    L.InR msList -> T.intercalate "\n" (map markedStringToText msList)

markedStringToText :: L.MarkedString -> Text
markedStringToText (L.MarkedString inner) = case inner of
  L.InL t -> t
  L.InR mswl -> mswl._value

extractDefinitions :: L.Definition L.|? ([L.DefinitionLink] L.|? L.Null) -> [L.Location]
extractDefinitions result = case result of
  L.InL (L.Definition locOrLocs) -> case locOrLocs of
    L.InL loc -> [loc]
    L.InR locs -> locs
  L.InR linksOrNull -> case linksOrNull of
    L.InL links -> map linkToLocation links
    L.InR L.Null -> []
  where
    linkToLocation (L.DefinitionLink link) = L.Location
      { L._uri = link._targetUri
      , L._range = link._targetRange
      }


-- ════════════════════════════════════════════════════════════════════════════
-- OUTPUT BUILDING
-- ════════════════════════════════════════════════════════════════════════════

buildOutput
  :: Maybe (Text, Text)  -- ^ Hover result (name, signature)
  -> [L.Location]        -- ^ Definitions
  -> [L.Location]        -- ^ References
  -> ImpactOutput
buildOutput mHover defs refs = ImpactOutput
  { symbol = fmap (\(name, sig) -> SymbolInfo name sig) mHover
  , definedAt = map toLocationInfo defs
  , references = map toLocationInfo refs
  , stats = computeStats refs
  }

toLocationInfo :: L.Location -> LocationInfo
toLocationInfo loc = LocationInfo
  { locFile = let L.Uri u = loc._uri in u
  , locLine = fromIntegral loc._range._start._line + 1  -- Back to 1-indexed
  , locCol = fromIntegral loc._range._start._character + 1
  }

computeStats :: [L.Location] -> ImpactStats
computeStats refs =
  let -- Group by file
      byFile = Map.fromListWith (+)
        [ (let L.Uri u = loc._uri in u, 1 :: Int)
        | loc <- refs
        ]
      -- Sort by count descending
      breakdown = sortBy (comparing (Down . snd)) $ Map.toList byFile
  in ImpactStats
    { totalReferences = length refs
    , filesAffected = Map.size byFile
    , fileBreakdown = breakdown
    }
