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
  , exportWithExpansion
  , discoverSymbols
  ) where

import Control.Monad (forM_, when, forM)
import Control.Monad.Freer (runM)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Data.List (nub)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (stdout, stderr, hFlush, hPutStrLn)

import Tidepool.Control.Scout.Teach.Gemma (extractCandidates)
import Tidepool.Control.Scout.LSP (locationToText)
import Tidepool.Effect.LSP
  ( workspaceSymbol, hover, textDocument, position
  , SymbolInformation(..), HoverInfo(..), Location(..), Range(..), Position(..)
  , SymbolKind(..)
  )
import Tidepool.LSP.Interpreter (LSPSession, runLSP)
import Tidepool.Training.Format (formatSelectSymbolsExample)

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

      let (sig, docs, code) = case maybeHover of
            Just hoverInfo ->
              let HoverInfo contents _ = hoverInfo
              in (contents, Nothing, Nothing)  -- TODO: Parse hover contents properly
            Nothing -> (symName, Nothing, Nothing)

      let candidates = extractCandidates sig

      -- Skip if no candidates (e.g., simple types with no references)
      when (not $ null candidates) $ do
        let location = locationToText loc
        let jsonl = formatSelectSymbolsExample
                      symName
                      location
                      sig
                      code
                      docs
                      candidates

        -- Output JSONL line
        BL.putStrLn jsonl
        hFlush stdout

-- | Discover interesting symbols in the workspace.
--
-- Filters to functions only, since data types have minimal hover info
-- (just ":: Type") while functions have full signatures with type references.
discoverSymbols :: LSPSession -> IO [Text]
discoverSymbols session = do
  allSyms <- runM $ runLSP session $ workspaceSymbol ""

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
    <> show (length allSyms) <> " workspace symbols"
  pure uniqueNames

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
            let (sig, docs, code) = case maybeHover of
                  Just hoverInfo ->
                    let HoverInfo contents _ = hoverInfo
                    in (contents, Nothing, Nothing)
                  Nothing -> (name, Nothing, Nothing)

            let candidates = extractCandidates sig

            -- Generate example if we have candidates
            when (not $ null candidates) $ do
              let location = locationToText loc
              let jsonl = formatSelectSymbolsExample name location sig code docs candidates
              BL.putStrLn jsonl
              hFlush stdout
              modifyIORef' countRef (+1)
              newCount <- readIORef countRef
              when (newCount `mod` 50 == 0) $
                hPutStrLn stderr $ "[Progress] " <> show newCount <> " examples generated"

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
