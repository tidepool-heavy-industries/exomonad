{-# LANGUAGE OverloadedStrings #-}

-- | Export training examples from LSP exploration.
--
-- Generates FunctionGemma training data in 3-turn token format with holes
-- for human annotation. Uses LSP (workspace/symbol, hover, definition) to
-- extract type signatures and follow type imports across packages.
--
-- Strategy: BFS crawl following type definitions
-- 1. Start with functions in current workspace
-- 2. For each candidate type, use go-to-definition to find source
-- 3. If definition is in a new file, add that file's functions to queue
-- 4. Continue until target example count reached
module Tidepool.Control.Export
  ( exportTrainingExamples
  , exportGroupedTrainingExamples
  , exportWithExpansion
  , discoverSymbols
  , findHaskellFiles
  , readCodeAtRange
  , sampleCodeBodies
  , exportCodeSamples
  , exportMCPTools
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, when, forM)
import Control.Monad.Freer (runM)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Data.List (nub)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), takeExtension)
import System.IO (stdout, stderr, hFlush, hPutStrLn)
import System.Random (randomRIO)

import Data.Aeson (Value)
import Data.Proxy (Proxy(..))

import Tidepool.Control.Protocol (ToolDefinition(..))
import Tidepool.Control.LSPTools
  ( FindCallersGraph, ShowFieldsGraph, ShowConstructorsGraph )
import Tidepool.Control.Scout.DocGen (TeachQuery(..))
import Tidepool.Control.Scout.Graph (DocGenGraph)
import Tidepool.Control.Scout.DocGen.Gemma (extractCandidates)
import Tidepool.Effect.LSP
  ( workspaceSymbol, hover, textDocument, position, references
  , SymbolInformation(..), HoverInfo(..), Location(..), Range(..), Position(..)
  , SymbolKind(..)
  )
import Tidepool.Graph.MCPReify (ReifyMCPTools(..), MCPToolInfo(..))
import Tidepool.LSP.Interpreter (LSPSession, runLSP)
import Tidepool.Training.Format (formatSelectSymbolsExample, formatSelectSymbolsExampleGrouped)
import Tidepool.Training.Types (CandidateGroups(..))

-- | Extract module name from hover info.
--
-- Looks for patterns like:
--   *Defined in 'Tidepool.Effect.LSP'*
--   *Defined at /path/to/Module.hs:42*
--
-- Falls back to extracting from file path if hover doesn't have module info.
extractModuleName :: Text -> Text -> Text
extractModuleName hoverText filePath =
  case extractFromDefinedIn hoverText of
    Just m -> m
    Nothing -> extractFromFilePath filePath
  where
    -- Pattern: *Defined in 'Module.Name'*
    extractFromDefinedIn t =
      let needle = "Defined in '"
      in case T.breakOn needle t of
           (_, rest) | T.null rest -> Nothing
           (_, rest) ->
             let afterNeedle = T.drop (T.length needle) rest
             in case T.breakOn "'" afterNeedle of
                  (moduleName, _) | not (T.null moduleName) -> Just moduleName
                  _ -> Nothing

    -- Extract module from path like .../src/Tidepool/Effect/LSP.hs
    extractFromFilePath p =
      let fileName = T.takeWhileEnd (/= '/') p
          baseName = T.dropEnd 3 fileName  -- Remove .hs
          -- Try to find src/ prefix for module path
          parts = T.splitOn "/" p
          afterSrc = dropWhile (/= "src") parts
      in case afterSrc of
           (_src : rest) ->
             let modPath = T.intercalate "." $ map (T.dropEnd 3) $ filter (T.isSuffixOf ".hs") rest
             in if T.null modPath then baseName else T.replace "/" "." $ T.intercalate "/" (init rest) <> "." <> baseName
           _ -> baseName

-- | Extract package name from hover info.
--
-- Looks for pattern: *(package-name-version)*
-- Returns just the package name without version.
extractPackageName :: Text -> Text
extractPackageName hoverText =
  case extractFromParens hoverText of
    Just pkg -> pkg
    Nothing -> "unknown"
  where
    extractFromParens t =
      -- Pattern: *(package-name-0.1.0.0)*
      let needle = "*("
      in case T.breakOn needle t of
           (_, rest) | T.null rest -> Nothing
           (_, rest) ->
             let afterNeedle = T.drop (T.length needle) rest
             in case T.breakOn ")*" afterNeedle of
                  (pkgVersion, _) | not (T.null pkgVersion) ->
                    -- Strip version: "tidepool-core-0.1.0.0" -> "tidepool-core"
                    Just $ stripVersion pkgVersion
                  _ -> Nothing

    -- Strip version from package name
    -- "tidepool-core-0.1.0.0" -> "tidepool-core"
    stripVersion pkg =
      let parts = T.splitOn "-" pkg
          -- Version parts start with digits, package parts don't
          isVersionPart p = not (T.null p) && T.head p >= '0' && T.head p <= '9'
          pkgParts = takeWhile (not . isVersionPart) parts
      in if null pkgParts then pkg else T.intercalate "-" pkgParts

-- | Extract first sentence of documentation from hover.
--
-- Returns Nothing if no docs found. Only returns real Haddock docs,
-- not signature fragments.
extractFirstSentenceDoc :: Text -> Maybe Text
extractFirstSentenceDoc hoverText =
  -- HLS hover format puts docs after the signature, typically after a blank line
  -- or following specific patterns. Be conservative - only extract if clearly docs.
  let allLines = T.lines hoverText
      -- Find docs section: lines after signature that look like prose
      docsSection = dropWhile isSignatureLine allLines
      docLines = filter isDocLine $ take 5 docsSection  -- Look at first 5 non-sig lines
  in case docLines of
       [] -> Nothing
       (firstDoc : _) ->
         let sentence = T.strip $ firstSentence firstDoc
         -- Reject if it still looks like code
         in if T.null sentence || looksLikeCode sentence
            then Nothing
            else Just sentence
  where
    -- A line is part of the signature/metadata
    isSignatureLine line =
      let stripped = T.strip line
      in T.null stripped
         || "```" `T.isPrefixOf` stripped
         || "*Defined" `T.isPrefixOf` stripped
         || "::" `T.isInfixOf` stripped
         || "=>" `T.isInfixOf` stripped
         || "forall" `T.isPrefixOf` stripped
         || "->" `T.isInfixOf` stripped
         || "(" `T.isPrefixOf` stripped && ")" `T.isSuffixOf` stripped

    -- A line looks like documentation prose
    isDocLine line =
      let stripped = T.strip line
      in not (T.null stripped)
         && not (isSignatureLine line)
         && T.length stripped > 10  -- Docs are usually longer
         && T.any (== ' ') stripped  -- Prose has spaces

    -- Reject if it still looks like code
    looksLikeCode t =
      "::" `T.isInfixOf` t
      || "=>" `T.isInfixOf` t
      || "->" `T.isInfixOf` t
      || "forall" `T.isInfixOf` t
      || T.all (\c -> c `elem` ("()[]{},:;'" :: String) || not (c == ' ')) t

    -- Extract first sentence (up to period or newline)
    firstSentence t =
      let (sentence, _) = T.breakOn ". " t
      in if T.length sentence < T.length t
         then sentence <> "."
         else T.takeWhile (/= '\n') t

-- | Clean signature by stripping markdown and metadata.
--
-- Removes:
--   - ```haskell code fences
--   - *Defined in/at ...* metadata
--   - *(package-version)* annotations
--   - Excessive whitespace
--
-- Preserves:
--   - newtype/data keywords (important semantic signal)
cleanSignature :: Text -> Text
cleanSignature sig =
  sig
    -- Remove markdown code fences
    & T.replace "```haskell\n" ""
    & T.replace "```haskell" ""
    & T.replace "\n```" ""
    & T.replace "```" ""
    -- Remove "Defined in/at" lines
    & removeDefinedLines
    -- Remove package version annotations
    & removePackageVersions
    -- Clean up whitespace (preserving newtype/data)
    & T.strip
    & normalizeWhitespace
  where
    -- Remove lines starting with *Defined
    removeDefinedLines t =
      T.unlines $ filter (not . isDefinedLine) $ T.lines t
      where
        isDefinedLine line =
          "*Defined in" `T.isInfixOf` line ||
          "*Defined at" `T.isInfixOf` line

    -- Remove *(package-version)* patterns
    removePackageVersions t =
      -- Simple heuristic: remove *(...)*
      let parts = T.splitOn "*(" t
      in case parts of
           [single] -> single
           (first : rest) -> first <> T.concat (map dropUntilClose rest)
           [] -> t
      where
        dropUntilClose s = case T.breakOn ")*" s of
          (_, after) -> T.drop 2 after

    -- Collapse multiple spaces/newlines, preserving semantic keywords
    normalizeWhitespace t =
      T.unwords $ filter (not . T.null) $ T.words t

-- | Parse signature to extract input types and output type.
--
-- For a signature like: foo :: Config -> Text -> IO User
-- Returns: (inputs: [Config, Text], output: [User])
--
-- Handles:
--   - Simple function types: A -> B -> C
--   - Monadic return types: IO A, Eff effs A
--   - Constraints: (Show a, Eq a) => a -> b
--   - Effect signatures: Member Foo effs => ...
parseSignatureTypes :: Text -> ([Text], [Text])
parseSignatureTypes sig =
  let -- Strip constraints (everything before =>)
      afterConstraints = case T.breakOn "=>" sig of
        (_, rest) | not (T.null rest) -> T.drop 2 rest  -- Skip "=>"
        _ -> sig

      -- Split by top-level arrows (not inside parens/brackets)
      parts = splitByArrow $ T.strip afterConstraints

      -- Last part is the output, rest are inputs
      (inputs, output) = case reverse parts of
        [] -> ([], [])
        [single] -> ([], extractTypes single)  -- No arrows = just output
        (out : rest) -> (concatMap extractTypes (reverse rest), extractTypes out)

  in (inputs, output)
  where
    -- Split by "->" at top level (not inside parens)
    splitByArrow :: Text -> [Text]
    splitByArrow t = go 0 "" (T.unpack t)
      where
        go :: Int -> String -> String -> [Text]
        go _ acc [] = [T.strip $ T.pack $ reverse acc]
        go depth acc ('-':'>':rest)
          | depth == 0 = T.strip (T.pack $ reverse acc) : go 0 "" rest
        go depth acc ('(':rest) = go (depth + 1) ('(':acc) rest
        go depth acc ('[':rest) = go (depth + 1) ('[':acc) rest
        go depth acc (')':rest) = go (max 0 (depth - 1)) (')':acc) rest
        go depth acc (']':rest) = go (max 0 (depth - 1)) (']':acc) rest
        go depth acc (c:rest) = go depth (c:acc) rest

    -- Extract type names from a type expression
    extractTypes :: Text -> [Text]
    extractTypes t =
      let cleaned = T.strip t
          -- Handle monadic wrappers: IO a, Eff effs a, Maybe a
          unwrapped = unwrapMonad cleaned
      in extractCandidates unwrapped

    -- Unwrap common monadic wrappers to get the inner type
    unwrapMonad :: Text -> Text
    unwrapMonad t
      | "IO " `T.isPrefixOf` t = T.drop 3 t
      | "Eff " `T.isPrefixOf` t = dropEffWrapper t
      | otherwise = t

    -- Drop "Eff effs " prefix
    dropEffWrapper t =
      let afterEff = T.drop 4 t  -- Drop "Eff "
          -- Find the end of the effs variable/type
          parts = T.words afterEff
      in case parts of
           (_effs : rest) -> T.unwords rest
           _ -> t

-- | Check if example is trivial (single candidate = symbol name).
isTrivialExample :: Text -> [Text] -> Bool
isTrivialExample symName candidates =
  case candidates of
    [single] -> single == symName
    _ -> False

-- | Helper for function application in pipes
(&) :: a -> (a -> b) -> b
(&) = flip ($)
infixl 1 &

-- | Reference cap - beyond this count, mark as hub symbol.
referenceCap :: Int
referenceCap = 20

-- | Extract references for a symbol, capping at referenceCap.
--
-- If references > cap, returns Left "[many references - hub symbol]"
-- Otherwise returns Right with list of location strings.
extractReferences
  :: LSPSession
  -> Text               -- ^ File URI
  -> Position           -- ^ Symbol position
  -> IO (Either Text [Text])
extractReferences session fileUri pos = do
  let doc = textDocument fileUri
  refs <- runM $ runLSP session $ references doc pos
  let refCount = length refs
  if refCount > referenceCap
    then pure $ Left $ "[many references (" <> T.pack (show refCount) <> ") - hub symbol]"
    else pure $ Right $ map locationToText refs
  where
    -- Format Location as "Module.hs:42" or similar
    locationToText (Location uri (Range (Position line _) _)) =
      let file = case T.stripPrefix "file://" uri of
            Just f -> T.takeWhileEnd (/= '/') f  -- Just filename
            Nothing -> uri
      in file <> ":" <> T.pack (show (line + 1))  -- 1-indexed for display


-- | Build CandidateGroups from LSP orchestration.
--
-- - Fields: Empty (documentSymbol not available in our LSP effect)
-- - Types: Extracted from signature via extractCandidates
-- - References: Via findReferences with cap
buildCandidateGroups
  :: LSPSession
  -> Text               -- ^ File URI
  -> Position           -- ^ Symbol position
  -> Text               -- ^ Raw signature/hover text
  -> IO CandidateGroups
buildCandidateGroups session fileUri pos rawSig = do
  -- Clean signature first, then parse to split inputs from output
  let cleaned = cleanSignature rawSig
      (inputs, output) = parseSignatureTypes cleaned

  -- References with cap
  refs <- extractReferences session fileUri pos

  pure CandidateGroups
    { cgFields = []  -- documentSymbol not available
    , cgInputs = inputs
    , cgOutput = output
    , cgReferences = refs
    }

-- | Export training examples for given seed symbols.
exportTrainingExamples
  :: LSPSession
  -> [Text]      -- Seed symbol names
  -> IO ()
exportTrainingExamples session seeds = do
  forM_ seeds $ \seedName -> do
    -- 1. Find symbol via LSP
    symbols <- runM $ runLSP session $ workspaceSymbol seedName

    -- 2. For each result, fetch hover and extract candidates
    forM_ symbols $ \symInfo -> do
      let SymbolInformation symName _ loc _ = symInfo
          Location uri rng = loc
          file = case T.stripPrefix "file://" uri of
            Just f -> f
            Nothing -> uri
          Range startPosRec _ = rng
          Position line char = startPosRec
          startPos = position line char

      -- Get hover info
      maybeHover <- runM $ runLSP session $ hover (textDocument file) startPos

      let rawSig = case maybeHover of
            Just hoverInfo ->
              let HoverInfo contents _ = hoverInfo
              in contents
            Nothing -> symName

      -- Extract metadata and clean signature
      let moduleName = extractModuleName rawSig file
          packageName = extractPackageName rawSig
          cleanedSig = cleanSignature rawSig
          maybeDocs = extractFirstSentenceDoc rawSig

      -- Extract candidates
      let candidates = extractCandidates rawSig

      -- Skip trivial examples (single candidate = symbol name)
      when (not (null candidates) && not (isTrivialExample symName candidates)) $ do
        let jsonl = formatSelectSymbolsExample
                      symName
                      moduleName
                      packageName
                      cleanedSig
                      maybeDocs
                      candidates

        -- Output JSONL line
        BL.putStrLn jsonl
        hFlush stdout

-- | Export training examples with grouped candidates (v2 format).
--
-- Uses LSP orchestration to build CandidateGroups:
-- - Inputs: argument types from signature (dependencies)
-- - Output: return type from signature (what this produces)
-- - References: from findReferences (capped at 20)
-- - Fields: empty (documentSymbol not in our LSP effect)
--
-- Output format includes grouped candidates for richer training signal.
exportGroupedTrainingExamples
  :: LSPSession
  -> [Text]      -- ^ Seed symbol names
  -> IO ()
exportGroupedTrainingExamples session seeds = do
  forM_ seeds $ \seedName -> do
    -- 1. Find symbol via LSP
    symbols <- runM $ runLSP session $ workspaceSymbol seedName

    -- 2. For each result, fetch hover and build grouped candidates
    forM_ symbols $ \symInfo -> do
      let SymbolInformation symName _ loc _ = symInfo
          Location uri rng = loc
          file = case T.stripPrefix "file://" uri of
            Just f -> f
            Nothing -> uri
          Range startPosRec _ = rng
          Position line char = startPosRec
          startPos = position line char

      -- Get hover info
      maybeHover <- runM $ runLSP session $ hover (textDocument file) startPos

      let rawSig = case maybeHover of
            Just hoverInfo ->
              let HoverInfo contents _ = hoverInfo
              in contents
            Nothing -> symName

      -- Extract metadata
      let moduleName = extractModuleName rawSig file
          packageName = extractPackageName rawSig
          cleanedSig = cleanSignature rawSig

      -- Build grouped candidates via LSP orchestration
      groups <- buildCandidateGroups session file startPos rawSig

      -- Skip if no types (references alone aren't useful for select_symbols)
      let hasInputs = not (null (cgInputs groups))
          hasOutput = not (null (cgOutput groups))
          hasRefs = case cgReferences groups of
            Left _ -> True  -- Hub symbols are interesting
            Right refs -> not (null refs)

      when (hasInputs || hasOutput || hasRefs) $ do
        let jsonl = formatSelectSymbolsExampleGrouped
                      symName
                      moduleName
                      packageName
                      cleanedSig
                      groups

        -- Output JSONL line
        BL.putStrLn jsonl
        hFlush stdout

-- | Common function prefixes to query for.
--
-- HLS limits workspaceSymbol results, so querying multiple patterns
-- helps discover more functions.
functionPrefixes :: [Text]
functionPrefixes =
  [ ""        -- All symbols
  , "run"     -- runX interpreters
  , "handle"  -- handlers
  , "mk"      -- smart constructors
  , "to"      -- conversion functions
  , "from"    -- conversion functions
  , "get"     -- getters
  , "set"     -- setters
  , "with"    -- bracketing functions
  , "parse"   -- parsers
  , "render"  -- renderers
  , "format"  -- formatters
  , "extract" -- extractors
  , "build"   -- builders
  , "create"  -- creators
  , "process" -- processors
  , "apply"   -- applicators
  , "eval"    -- evaluators
  , "exec"    -- executors
  ]

-- | Discover interesting symbols in the workspace.
--
-- Filters to functions only, since data types have minimal hover info
-- (just ":: Type") while functions have full signatures with type references.
discoverSymbols :: LSPSession -> IO [Text]
discoverSymbols session = do
  -- Query multiple patterns to overcome HLS result limits
  allSymLists <- forM functionPrefixes $ \prefix -> do
    runM $ runLSP session $ workspaceSymbol prefix

  let allSyms = concat allSymLists

  -- Filter to functions only (SKFunction, SKMethod, SKVariable)
  -- Data types (SKClass, SKStruct, SKEnum) just show ":: Type"
  let isFunctionLike (SymbolInformation _ kind _ _) = kind `elem` [SKFunction, SKMethod, SKVariable]
      functions = filter isFunctionLike allSyms

  -- Filter out derived instances ($fShow*, $fEq*, etc.)
  let isNotDerived (SymbolInformation name _ _ _) = not ("$f" `T.isPrefixOf` name)
      userDefined = filter isNotDerived functions

  -- Extract unique names (no limit - we want 800-1k examples)
  let uniqueNames = nub $ map (\(SymbolInformation name _ _ _) -> name) userDefined

  hPutStrLn stderr $ "Discovered " <> show (length uniqueNames) <> " functions from "
    <> show (length allSyms) <> " total symbol results"
  pure uniqueNames

-- | Key files to open to trigger HLS multi-package indexing.
--
-- Opening a file from each package triggers HLS to load that component.
triggerFiles :: [Text]
triggerFiles =
  [ "haskell/dsl/core/src/Tidepool/Graph/Types.hs"
  , "haskell/control-server/src/Tidepool/Control/Server.hs"
  , "haskell/effects/llm-interpreter/src/Tidepool/LLM/Interpreter.hs"
  , "haskell/effects/lsp-interpreter/src/Tidepool/LSP/Interpreter.hs"
  , "haskell/runtime/actor/src/Tidepool/Actor/Types.hs"
  , "haskell/native-server/src/Tidepool/Native/Server.hs"
  , "haskell/effects/mcp-server/src/Tidepool/MCP/Server.hs"
  , "haskell/tools/training-generator/src/Tidepool/Training/Types.hs"
  ]

-- | Export with automatic expansion by following type definitions.
--
-- BFS crawl: starts with local functions, follows candidate types to their
-- definitions in other packages, continues until target count reached.
exportWithExpansion
  :: LSPSession
  -> Int           -- ^ Target example count (e.g., 1000)
  -> IO ()
exportWithExpansion session targetCount = do
  -- Track state
  countRef <- newIORef (0 :: Int)
  visitedFilesRef <- newIORef Set.empty
  pendingTypesRef <- newIORef Set.empty

  -- Trigger HLS to index multiple packages by opening key files
  hPutStrLn stderr "Opening key files to trigger multi-package indexing..."
  forM_ triggerFiles $ \file -> do
    hPutStrLn stderr $ "  Opening: " <> T.unpack file
    -- Hover at line 1 forces HLS to load the file's component
    _ <- runM $ runLSP session $ hover (textDocument ("file://" <> file)) (position 0 0)
    pure ()

  -- Give HLS time to index the newly loaded components
  hPutStrLn stderr "Waiting for HLS to index new components (5 seconds)..."
  threadDelay (5 * 1000000)

  -- Start with local workspace symbols
  initialSyms <- discoverSymbols session

  hPutStrLn stderr $ "Starting expansion with " <> show (length initialSyms) <> " seed symbols"
  hPutStrLn stderr $ "Target: " <> show targetCount <> " examples"

  -- Process initial symbols, collecting candidate types
  forM_ initialSyms $ \seedName -> do
    count <- readIORef countRef
    when (count < targetCount) $ do
      newTypes <- processSymbol session seedName countRef visitedFilesRef targetCount
      -- Add discovered types to pending queue
      forM_ newTypes $ \t -> modifyIORef' pendingTypesRef (Set.insert t)

  -- BFS: follow pending types to their definitions
  let expandLoop = do
        count <- readIORef countRef
        pending <- readIORef pendingTypesRef
        when (count < targetCount && not (Set.null pending)) $ do
          -- Pop a type from pending
          let (nextType, remaining) = Set.deleteFindMin pending
          writeIORef pendingTypesRef remaining

          hPutStrLn stderr $ "[Expand] Following type: " <> T.unpack nextType

          -- Find definition of this type
          maybeFile <- findTypeDefinitionFile session nextType
          case maybeFile of
            Nothing -> expandLoop  -- Type not found, continue
            Just file -> do
              visited <- readIORef visitedFilesRef
              if file `Set.member` visited
                then expandLoop  -- Already visited
                else do
                  modifyIORef' visitedFilesRef (Set.insert file)
                  hPutStrLn stderr $ "[Expand] New file: " <> T.unpack file

                  -- Find functions in this file via workspace symbol
                  -- Query for common function prefixes found in the file
                  let fileName = T.takeWhileEnd (/= '/') file
                      baseName = T.dropEnd 3 fileName  -- Remove .hs
                  funcs <- runM $ runLSP session $ workspaceSymbol baseName
                  let funcNames = [n | SymbolInformation n k _ _ <- funcs
                                     , k `elem` [SKFunction, SKMethod, SKVariable]
                                     , not ("$f" `T.isPrefixOf` n)]
                  hPutStrLn stderr $ "[Expand] Found " <> show (length funcNames) <> " functions"

                  -- Process each function
                  forM_ funcNames $ \funcName -> do
                    c <- readIORef countRef
                    when (c < targetCount) $ do
                      newTypes <- processSymbol session funcName countRef visitedFilesRef targetCount
                      forM_ newTypes $ \t -> modifyIORef' pendingTypesRef (Set.insert t)

                  expandLoop

  expandLoop

  finalCount <- readIORef countRef
  hPutStrLn stderr $ "Expansion complete: " <> show finalCount <> " examples generated"

-- | Process a single symbol: generate example and return candidate types.
processSymbol
  :: LSPSession
  -> Text           -- ^ Symbol name
  -> IORef Int      -- ^ Counter ref
  -> IORef (Set.Set Text)  -- ^ Visited files ref
  -> Int            -- ^ Target count
  -> IO [Text]      -- ^ Returns candidate types for expansion
processSymbol session symName countRef visitedFilesRef targetCount = do
  count <- readIORef countRef
  if count >= targetCount
    then pure []
    else do
      symbols <- runM $ runLSP session $ workspaceSymbol symName
      candidatesLists <- forM symbols $ \symInfo -> do
        c <- readIORef countRef
        if c >= targetCount
          then pure []
          else do
            let SymbolInformation name _ loc _ = symInfo
                Location uri rng = loc
                file = case T.stripPrefix "file://" uri of
                  Just f -> f
                  Nothing -> uri
                Range startPosRec _ = rng
                Position line char = startPosRec
                startPos = position line char

            -- Mark file as visited
            modifyIORef' visitedFilesRef (Set.insert file)

            -- Get hover info
            maybeHover <- runM $ runLSP session $ hover (textDocument file) startPos
            let rawSig = case maybeHover of
                  Just hoverInfo ->
                    let HoverInfo contents _ = hoverInfo
                    in contents
                  Nothing -> name

            -- Extract metadata and clean signature
            let moduleName = extractModuleName rawSig file
                packageName = extractPackageName rawSig
                cleanedSig = cleanSignature rawSig
                maybeDocs = extractFirstSentenceDoc rawSig

            -- Extract candidates
            let candidates = extractCandidates rawSig

            -- Generate example if we have non-trivial candidates
            when (not (null candidates) && not (isTrivialExample name candidates)) $ do
              let jsonl = formatSelectSymbolsExample name moduleName packageName cleanedSig maybeDocs candidates
              BL.putStrLn jsonl
              hFlush stdout
              modifyIORef' countRef (+1)
              newCount <- readIORef countRef
              when (newCount `mod` 50 == 0) $
                hPutStrLn stderr $ "[Progress] " <> show newCount <> " examples generated"

            -- Return candidates for expansion
            pure candidates

      pure $ nub $ concat candidatesLists

-- | Find the file where a type is defined using go-to-definition.
findTypeDefinitionFile :: LSPSession -> Text -> IO (Maybe Text)
findTypeDefinitionFile session typeName = do
  -- Search for the type in workspace
  symbols <- runM $ runLSP session $ workspaceSymbol typeName
  case symbols of
    [] -> pure Nothing
    (SymbolInformation _ _ (Location uri _) _ : _) ->
      pure $ Just $ case T.stripPrefix "file://" uri of
        Just f -> f
        Nothing -> uri


-- ════════════════════════════════════════════════════════════════════════════
-- CODE SAMPLING (V3: Code-Native Training Format)
-- ════════════════════════════════════════════════════════════════════════════

-- | Find all Haskell source files in a directory tree.
--
-- Recursively traverses directories, collecting *.hs files.
-- Excludes build artifacts: dist-newstyle, .stack-work, vendor, .git
findHaskellFiles :: FilePath -> IO [FilePath]
findHaskellFiles root = do
  isDir <- doesDirectoryExist root
  if not isDir
    then pure []
    else do
      entries <- listDirectory root
      let filteredEntries = filter (not . isExcluded) entries
      paths <- forM filteredEntries $ \entry -> do
        let fullPath = root </> entry
        isFile <- doesFileExist fullPath
        isDirectory <- doesDirectoryExist fullPath
        if isFile && takeExtension fullPath == ".hs"
          then pure [fullPath]
          else if isDirectory
            then findHaskellFiles fullPath
            else pure []
      pure $ concat paths
  where
    isExcluded :: FilePath -> Bool
    isExcluded name = name `elem`
      [ "dist-newstyle", ".stack-work", "vendor", ".git"
      , "dist", "build", ".cabal-sandbox"
      ]

-- | Read code from file at LSP Range, with context expansion.
--
-- For training data, we need full function bodies, not just signatures.
-- Strategy: Read from startLine until we hit a function boundary:
--   1. Next top-level definition (column 0, alphanumeric start)
--   2. Section divider (═══ or ---)
--   3. Haddock comment for new definition (unindented "-- |")
--   4. Two consecutive blank lines
--   5. Maximum 30 lines (hard limit)
--
-- LSP Positions are 0-indexed.
readCodeAtRange :: FilePath -> Range -> IO Text
readCodeAtRange file (Range (Position startLine _) (Position endLine _)) = do
  contents <- TIO.readFile file
  let allLines = T.lines contents
      startIdx = fromIntegral startLine
      endIdx = fromIntegral endLine

      -- Read up to 30 lines past the LSP range
      maxIdx = min (length allLines - 1) (endIdx + 30)
      candidateLines = drop startIdx $ take (maxIdx + 1) allLines

      -- Detect HARD boundaries (always stop regardless of state)
      isHardBoundary line =
        -- Section divider (═══ or ---)
        "═" `T.isInfixOf` line ||
        (T.isPrefixOf "--" line && T.length line > 10 && T.all (\c -> c == '-' || c == ' ') (T.drop 2 line))

      -- Detect soft boundaries (type sig, haddock) - only after we've seen code
      isSoftBoundary line =
        -- Haddock for next definition (unindented "-- |")
        T.isPrefixOf "-- |" line ||
        -- Next type signature at column 0 (contains "::")
        (not (T.null line) &&
         not (T.isPrefixOf " " line) &&
         not (T.isPrefixOf "\t" line) &&
         "::" `T.isInfixOf` line)

      -- Is this a "preamble" line (haddock or type signature)?
      isPreamble line =
        T.isPrefixOf "-- |" line ||
        T.isPrefixOf "-- " line ||  -- continuation haddock
        (not (T.null line) && not (T.isPrefixOf " " line) && "::" `T.isInfixOf` line)

      -- Check for two consecutive blank lines
      hasDoubleBlank [] = False
      hasDoubleBlank [_] = False
      hasDoubleBlank (a:b:_) = T.null (T.strip a) && T.null (T.strip b)

      -- Take lines until boundary
      -- seenCode tracks whether we've passed the preamble (haddock + type sig)
      -- Soft boundaries (type sig, haddock) only count after we've seen actual code
      takeUntilBoundary :: [Text] -> [Text] -> Bool -> [Text]
      takeUntilBoundary [] _ _ = []
      takeUntilBoundary (l:ls) prev seenCode
        | hasDoubleBlank (prev ++ [l]) = []           -- Stop at double blank
        | isHardBoundary l = []                        -- Always stop at hard boundary
        | seenCode && isSoftBoundary l = []           -- Stop at soft boundary only after code
        | otherwise =
            let nowSeenCode = seenCode || (not (isPreamble l) && not (T.null (T.strip l)))
            in l : takeUntilBoundary ls [l] nowSeenCode

      bodyLines = case candidateLines of
        [] -> []
        (firstLine:rest) ->
          let startSeenCode = not (isPreamble firstLine) && not (T.null (T.strip firstLine))
          in firstLine : takeUntilBoundary rest [firstLine] startSeenCode

  pure $ T.unlines bodyLines

-- | Sample random code bodies from entire codebase.
--
-- Strategy:
-- 1. Use workspaceSymbol to discover all symbols efficiently (HLS has this indexed)
-- 2. Filter to function-like symbols with valid locations
-- 3. Randomly sample N symbols from collected set
-- 4. Read code at each symbol's range
--
-- Returns: [(Text, Range, SymbolName, CodeBody)] where first Text is file path
sampleCodeBodies
  :: LSPSession
  -> FilePath    -- ^ Project root (unused, kept for API compatibility)
  -> Int         -- ^ Number to sample
  -> IO [(Text, Range, Text, Text)]
sampleCodeBodies session _projectRoot count = do
  -- Use workspaceSymbol for fast discovery (much faster than documentSymbol on all files)
  hPutStrLn stderr "[Sample] Discovering symbols via LSP workspaceSymbol..."
  allSymNames <- discoverSymbols session

  hPutStrLn stderr $ "[Sample] Found " <> show (length allSymNames) <> " symbols"

  -- Get full location info for each symbol
  hPutStrLn stderr "[Sample] Resolving symbol locations..."
  symbolsWithLocations <- fmap concat $ forM allSymNames $ \symName -> do
    syms <- runM $ runLSP session $ workspaceSymbol symName
    -- Filter to exact matches with valid locations
    let matching = [(file, range, name)
                   | SymbolInformation name kind (Location uri range) _ <- syms
                   , name == symName
                   , kind `elem` [SKFunction, SKMethod, SKVariable, SKClass, SKStruct]
                   , not (T.null uri)
                   , let file = case T.stripPrefix "file://" uri of
                           Just f -> f
                           Nothing -> uri
                   ]
    pure matching

  hPutStrLn stderr $ "[Sample] Resolved " <> show (length symbolsWithLocations) <> " symbol locations"

  -- Randomly sample N symbols
  let sampleCount = min count (length symbolsWithLocations)
  hPutStrLn stderr $ "[Sample] Sampling " <> show sampleCount <> " random symbols..."
  sampled <- randomSample sampleCount symbolsWithLocations

  -- Read code at each range
  hPutStrLn stderr "[Sample] Reading code bodies..."
  forM sampled $ \(file, range, name) -> do
    code <- readCodeAtRange (T.unpack file) range
    pure (file, range, name, code)

-- | Randomly sample N items from a list.
randomSample :: Int -> [a] -> IO [a]
randomSample n items
  | n >= length items = pure items
  | n <= 0 = pure []
  | otherwise = go n items []
  where
    go 0 _ acc = pure acc
    go _ [] acc = pure acc  -- Pool exhausted
    go remaining pool acc = do
      idx <- randomRIO (0, length pool - 1)
      case splitAt idx pool of
        (before, chosen:after) -> do
          let newPool = before ++ after
          go (remaining - 1) newPool (chosen : acc)
        _ -> pure acc  -- Should never happen given bounds, but satisfies exhaustiveness

-- | Export code-based training skeleton.
--
-- Generates JSONL with code bodies and placeholders for annotation:
-- {"file": "...", "range": {...}, "name": "...", "code": "...",
--  "criteria": "TODO: add criteria", "selected": []}
exportCodeSamples :: LSPSession -> FilePath -> Int -> IO ()
exportCodeSamples session projectRoot count = do
  samples <- sampleCodeBodies session projectRoot count
  hPutStrLn stderr $ "[Export] Generated " <> show (length samples) <> " samples"

  forM_ samples $ \(file, Range (Position startLine startChar) (Position endLine endChar), name, code) -> do
    let skeleton = object
          [ "file" .= file
          , "range" .= object
              [ "start" .= object
                  [ "line" .= startLine
                  , "character" .= startChar
                  ]
              , "end" .= object
                  [ "line" .= endLine
                  , "character" .= endChar
                  ]
              ]
          , "name" .= name
          , "code" .= code
          , "criteria" .= ("TODO: add criteria" :: Text)
          , "selected" .= ([] :: [Text])
          ]
    BL.putStrLn $ encode skeleton
    hFlush stdout
-- ════════════════════════════════════════════════════════════════════════════
-- MCP TOOL AUTO-DISCOVERY
-- ════════════════════════════════════════════════════════════════════════════

-- | Export all MCP tools from graph DSL annotations.
--
-- Uses ReifyMCPTools to extract tool metadata from MCPExport annotations.
-- Returns ToolDefinition format that matches Rust protocol types.
exportMCPTools :: IO [ToolDefinition]
exportMCPTools = do
  -- Extract all tools from graph DSL (automatic discovery)
  let allTools = concat
        [ reifyMCPTools (Proxy @FindCallersGraph)
        , reifyMCPTools (Proxy @ShowFieldsGraph)
        , reifyMCPTools (Proxy @ShowConstructorsGraph)
        , reifyMCPTools (Proxy @DocGenGraph)
        ]

  pure $ map reifyToToolDef allTools

-- | Convert MCPToolInfo -> ToolDefinition.
reifyToToolDef :: MCPToolInfo -> ToolDefinition
reifyToToolDef (MCPToolInfo name desc schema _entryName) = ToolDefinition
  { tdName = name
  , tdDescription = desc
  , tdInputSchema = schema
  }
