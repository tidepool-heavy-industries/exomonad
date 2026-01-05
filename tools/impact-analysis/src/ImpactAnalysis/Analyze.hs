-- | Core analysis logic using LSP
--
-- Queries a language server for symbol information at a given position,
-- including hover info, definition location, and all references.
--
module ImpactAnalysis.Analyze
  ( runAnalysis
  , findProjectRoot
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, try, SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.List (isSuffixOf, sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing, Down(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import System.Directory (makeAbsolute, doesFileExist, listDirectory)
import System.FilePath (takeDirectory, (</>))
import System.IO (Handle, BufferMode(..), hSetBuffering)
import System.Process (CreateProcess(..), StdStream(..), ProcessHandle, createProcess, proc, terminateProcess, cwd)

import Language.LSP.Client (runSessionWithHandles)
import Language.LSP.Client.Session (Session, initialize, request, openDoc)
import qualified Language.LSP.Protocol.Types as L
import qualified Language.LSP.Protocol.Message as L

import ImpactAnalysis.Types


-- ════════════════════════════════════════════════════════════════════════════
-- PUBLIC API
-- ════════════════════════════════════════════════════════════════════════════

-- | Run full impact analysis for a symbol at the given position.
--
-- @
-- runAnalysis ImpactInput
--   { file = "src/Foo.hs"
--   , line = 42
--   , col = 10
--   }
-- @
runAnalysis :: ImpactInput -> IO (Either Text ImpactOutput)
runAnalysis input = do
  absPath <- makeAbsolute (file input)
  mProjectRoot <- findProjectRoot absPath

  case mProjectRoot of
    Nothing -> pure $ Left $ "Could not find project root for: " <> T.pack absPath
    Just projectRoot -> do
      result <- try $ withHLSSession projectRoot $ \hls ->
        runSessionWithHandles hls.hlsStdout hls.hlsStdin $ do
          _ <- initialize Nothing

          -- Open document (REQUIRED before any requests)
          doc <- openDoc absPath "haskell"

          -- Wait for HLS to typecheck (2 seconds should be enough for most files)
          liftIO $ threadDelay 2000000

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
      | dir == "/" = pure Nothing
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

withHLSSession :: FilePath -> (HLSSession -> IO a) -> IO a
withHLSSession projectRoot = bracket acquire release
  where
    acquire = do
      (Just stdin', Just stdout', Just _stderr', ph) <- createProcess
        (proc "haskell-language-server-wrapper" ["--lsp"])
          { std_in = CreatePipe
          , std_out = CreatePipe
          , std_err = CreatePipe
          , cwd = Just projectRoot
          }
      hSetBuffering stdin' NoBuffering
      hSetBuffering stdout' NoBuffering
      pure $ HLSSession stdin' stdout' ph

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
