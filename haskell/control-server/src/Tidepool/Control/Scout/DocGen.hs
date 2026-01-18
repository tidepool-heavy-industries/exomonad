{-# LANGUAGE FlexibleContexts #-}

-- | Teaching document generation via LSP + Gemma.
--
-- Given a topic and seed symbols, explores the codebase to generate
-- a scouting document that explains the topic in BFS depth order.
--
-- Architecture:
--
-- @
-- TeachQuery { topic, seeds, budget }
--     │
--     ▼
-- ┌─────────────────────────────────────────┐
-- │           Exploration Loop              │
-- │                                         │
-- │  1. Find seed symbols (workspace/symbol)│
-- │  2. BFS: for each (symbol, depth):      │
-- │     a. Lookup LSP data (hover, location)│
-- │     b. Get related symbols (Gemma)      │
-- │     c. Resolve tokens → add to frontier │
-- │     d. Log failures, continue           │
-- │  3. Group by BFS depth                  │
-- │                                         │
-- └─────────────────────────────────────────┘
--     │
--     ▼
-- TeachingDoc { core, prereqs, support }
--   • core: depth 0 (seed symbols)
--   • prereqs: depth 1-2
--   • support: depth 3+
-- @
--
-- Key insight: Gemma extracts edges (related symbols). Haskell traverses
-- the graph via BFS, and depth determines scouting order.
module Tidepool.Control.Scout.DocGen
  ( -- * Main Entry Point
    scout

    -- * Configuration
  , TeachConfig(..)
  , defaultTeachConfig

    -- * Re-exports
  , module Tidepool.Control.Scout.DocGen.Types
  ) where

import Control.Monad (forM)
import Control.Monad.Freer (Eff, Member, LastMember)
import Data.List (sortOn, partition)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Tidepool.Control.Scout.DocGen.Types
import Tidepool.Control.Scout.DocGen.Gemma (ScoutGemma, selectRelevantSymbols, extractCandidates)
import Tidepool.Effect.Types (Log, logDebug)
import Tidepool.Effect.LSP
  ( LSP, workspaceSymbol, hover
  , textDocument, position
  , SymbolInformation(..), Location(..), Range(..), Position(..), HoverInfo(..)
  )
import Tidepool.Platform (NativeOnly)


-- ════════════════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Configuration for scouting exploration.
data TeachConfig = TeachConfig
  { tcDefaultBudget :: Int
    -- ^ Default budget if not specified (default: 20)
  , tcMaxMentionsPerSymbol :: Int
    -- ^ Max mentions to follow per symbol (default: 5)
  } deriving (Show, Eq)

defaultTeachConfig :: TeachConfig
defaultTeachConfig = TeachConfig
  { tcDefaultBudget = 20
  , tcMaxMentionsPerSymbol = 5
  }


-- ════════════════════════════════════════════════════════════════════════════
-- MAIN ENTRY POINT
-- ════════════════════════════════════════════════════════════════════════════

-- | Generate a scouting document from a query.
--
-- This is the main entry point. It:
--   1. Resolves seed symbols via LSP workspace/symbol
--   2. Explores the graph using BFS with Gemma token extraction
--   3. Groups by BFS depth (core/prereqs/support)
scout
  :: ( Member LSP effs
     , Member ScoutGemma effs
     , Member Log effs
     , LastMember IO effs
     , NativeOnly
     )
  => TeachConfig
  -> TeachQuery
  -> Eff effs TeachingDoc
scout config query = do
  logDebug $ "[Teach] Starting exploration for topic: " <> tqTopic query
  logDebug $ "[Teach] Seeds: " <> T.intercalate ", " (tqSeeds query)
  logDebug $ "[Teach] Budget: " <> T.pack (show (tqBudget query))

  -- 1. Resolve seed symbols to SymbolKeys
  seedKeys <- resolveSeedSymbols (tqSeeds query)
  logDebug $ "[Teach] Resolved " <> T.pack (show (length seedKeys)) <> " seed symbols"

  -- If no seeds found, return empty doc
  if null seedKeys
    then pure $ emptyDoc query
    else do
      -- 2. Initialize exploration state
      let budget = if tqBudget query <= 0 then tcDefaultBudget config else tqBudget query
      let initialState = initialTeachState seedKeys budget

      -- 3. Run exploration loop
      finalState <- exploreLoop config query initialState
      logDebug $ "[Teach] Exploration complete: " <> T.pack (show (Map.size (tsGraph finalState))) <> " symbols"

      -- 4. Build scouting document
      let doc = buildTeachingDoc query (tsGraph finalState)
      logDebug $ "[Teach] Generated doc with "
        <> T.pack (show (length (tdCore doc))) <> " core, "
        <> T.pack (show (length (tdPrereqs doc))) <> " prereqs, "
        <> T.pack (show (length (tdSupport doc))) <> " support"

      pure doc


-- ════════════════════════════════════════════════════════════════════════════
-- SEED RESOLUTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Resolve seed symbol names to SymbolKeys via LSP.
resolveSeedSymbols
  :: (Member LSP effs, Member Log effs, NativeOnly)
  => [Text]
  -> Eff effs [SymbolKey]
resolveSeedSymbols seeds = do
  results <- forM seeds $ \seedName -> do
    symbols <- workspaceSymbol seedName
    logDebug $ "[Teach] workspaceSymbol \"" <> seedName <> "\" -> "
      <> T.pack (show (length symbols)) <> " results"
    case symbols of
      [] -> do
        logDebug $ "[Teach] WARNING: No symbols found for seed: " <> seedName
        pure Nothing
      (sym:_) -> do
        let loc = sym.siLocation
            file = stripFilePrefix loc.locUri
        pure $ Just (T.unpack file, sym.siName)

  pure $ catMaybes results


-- ════════════════════════════════════════════════════════════════════════════
-- EXPLORATION LOOP
-- ════════════════════════════════════════════════════════════════════════════

-- | The core exploration loop (BFS).
--
-- Processes symbols from frontier until budget exhausted or frontier empty.
-- Each iteration:
--   1. Pop (symbol, depth) from frontier
--   2. Skip if already visited
--   3. Lookup LSP data (hover, location)
--   4. Get related symbols from Gemma
--   5. Resolve tokens → add to frontier with depth+1
--   6. Log failures and continue (graceful degradation)
exploreLoop
  :: ( Member LSP effs
     , Member ScoutGemma effs
     , Member Log effs
     , LastMember IO effs
     , NativeOnly
     )
  => TeachConfig
  -> TeachQuery
  -> TeachState
  -> Eff effs TeachState
exploreLoop config query state
  -- Budget exhausted
  | tsBudget state <= 0 = do
      logDebug "[Teach] Budget exhausted"
      pure state
  -- Frontier empty
  | null (tsFrontier state) = do
      logDebug "[Teach] Frontier empty"
      pure state
  -- Process next symbol
  | otherwise = do
      -- Pop from frontier (FIFO for BFS)
      let ((symKey, depth), frontier') = case tsFrontier state of
            (x:xs) -> (x, xs)
            []     -> error "impossible: checked null above"

      -- Skip if already visited
      if symKey `Set.member` tsVisited state
        then do
          logDebug $ "[Teach] Skipping already visited: " <> snd symKey
          exploreLoop config query state { tsFrontier = frontier' }
        else do
          let (file, name) = symKey
          logDebug $ "[Teach] Processing: " <> name
            <> " (depth=" <> T.pack (show depth)
            <> ", budget=" <> T.pack (show (tsBudget state)) <> ")"

          -- Lookup LSP data
          lspSymMaybe <- lookupSymbol symKey
          case lspSymMaybe of
            Nothing -> do
              logDebug $ "[Teach] LSP lookup failed for: " <> name
              -- Mark as visited but don't add to graph
              let state' = state
                    { tsFrontier = frontier'
                    , tsVisited = Set.insert symKey (tsVisited state)
                    }
              exploreLoop config query state'

            Just lspSym -> do
              -- Add to graph with depth
              let graph' = Map.insert symKey (depth, lspSym) (tsGraph state)

              -- Extract candidates deterministically from signature
              let candidates = extractCandidates (lsSignature lspSym)
              -- Select relevant symbols from candidates via Gemma
              selectedTokens <- selectRelevantSymbols (tqTopic query) lspSym candidates
              let tokensLimited = take (tcMaxMentionsPerSymbol config) selectedTokens

              logDebug $ "[Teach] -> related tokens: " <> T.intercalate ", " tokensLimited

              -- Resolve tokens to SymbolKeys (graceful: log failures)
              resolvedKeys <- resolveTokens file tokensLimited

              -- Filter to unseen symbols
              let unseen = filter (\k -> not (k `Set.member` tsVisited state)) resolvedKeys
              let newFrontier = map (\k -> (k, depth + 1)) unseen

              logDebug $ "[Teach] -> resolved " <> T.pack (show (length resolvedKeys))
                <> " (" <> T.pack (show (length unseen)) <> " new)"

              let state' = state
                    { tsGraph = graph'
                    , tsFrontier = frontier' ++ newFrontier  -- Append for BFS
                    , tsVisited = Set.insert symKey (tsVisited state)
                    , tsBudget = tsBudget state - 1
                    }

              exploreLoop config query state'


-- | Resolve token names to SymbolKeys.
--
-- Graceful degradation: logs failures and continues.
resolveTokens
  :: (Member LSP effs, Member Log effs, NativeOnly)
  => FilePath   -- ^ Context file (unused, but kept for future heuristics)
  -> [Text]     -- ^ Token names
  -> Eff effs [SymbolKey]
resolveTokens _contextFile tokens = do
  results <- forM tokens $ \token -> do
    symbols <- workspaceSymbol token
    case symbols of
      [] -> do
        logDebug $ "[Teach] WARNING: Could not resolve token: " <> token
        pure Nothing
      (sym:_) -> do
        let loc = sym.siLocation
            file = stripFilePrefix loc.locUri
        pure $ Just (T.unpack file, sym.siName)

  pure $ catMaybes results


-- ════════════════════════════════════════════════════════════════════════════
-- LSP LOOKUP
-- ════════════════════════════════════════════════════════════════════════════

-- | Lookup a symbol's LSP data.
lookupSymbol
  :: (Member LSP effs, NativeOnly)
  => SymbolKey
  -> Eff effs (Maybe LSPSymbol)
lookupSymbol (file, name) = do
  -- Search for the symbol
  symbols <- workspaceSymbol name
  case filter (\s -> T.unpack (stripFilePrefix s.siLocation.locUri) == file) symbols of
    [] -> do
      -- Fall back to first match if no file match
      case symbols of
        [] -> pure Nothing
        (sym:_) -> symbolToLSPSymbol sym

    (sym:_) -> symbolToLSPSymbol sym


-- | Convert SymbolInformation to LSPSymbol with hover data.
symbolToLSPSymbol
  :: (Member LSP effs, NativeOnly)
  => SymbolInformation
  -> Eff effs (Maybe LSPSymbol)
symbolToLSPSymbol sym = do
  let loc = sym.siLocation
      file = stripFilePrefix loc.locUri
      startPos = loc.locRange.rangeStart

  -- Get hover info at symbol location
  let doc = textDocument file
      pos = position startPos.posLine startPos.posCharacter

  hoverInfo <- hover doc pos
  let (sig, docComment) = case hoverInfo of
        Just h  -> parseHoverContent h.hoverContents
        Nothing -> (sym.siName, Nothing)

  pure $ Just LSPSymbol
    { lsName = sym.siName
    , lsKind = sym.siKind
    , lsLocation = loc
    , lsSignature = sig
    , lsDocComment = docComment
    }


-- | Parse hover content into signature and doc comment.
--
-- Hover content typically has format:
-- ```haskell
-- signature :: Type
-- ```
-- Documentation comment here
parseHoverContent :: Text -> (Text, Maybe Text)
parseHoverContent content =
  let lns = T.lines content
      -- Find code block
      (codeLines, docLines) = span (/= "```") (drop 1 $ dropWhile (/= "```haskell") lns)
      sig = T.strip $ T.unlines codeLines
      doc = case dropWhile (== "```") docLines of
        [] -> Nothing
        ds -> let d = T.strip $ T.unlines ds
              in if T.null d then Nothing else Just d
  in if T.null sig
     then (content, Nothing)  -- Fall back to raw content
     else (sig, doc)


-- ════════════════════════════════════════════════════════════════════════════
-- DOCUMENT BUILDING
-- ════════════════════════════════════════════════════════════════════════════

-- | Build a scouting document from the exploration graph.
--
-- Groups by BFS depth:
--   - Depth 0: Core (seed symbols)
--   - Depth 1-2: Prerequisites
--   - Depth 3+: Support
buildTeachingDoc :: TeachQuery -> Map SymbolKey (Int, LSPSymbol) -> TeachingDoc
buildTeachingDoc query graph =
  let -- Convert to list of TeachingUnits
      units = map toUnit (Map.toList graph)

      -- Sort by depth, then name
      sorted = sortOn (\u -> (tuDepth u, lsName (tuSymbol u))) units

      -- Partition by depth
      (core, rest) = partition (\u -> tuDepth u == 0) sorted
      (prereqs, support) = partition (\u -> tuDepth u <= 2) rest

  in TeachingDoc
    { tdTitle = "Understanding: " <> tqTopic query
    , tdTopic = tqTopic query
    , tdCore = core
    , tdPrereqs = prereqs
    , tdSupport = support
    }
  where
    toUnit (_, (depth, lsp)) = TeachingUnit
      { tuSymbol = lsp
      , tuDepth = depth
      }


-- | Create an empty document when no seeds found.
emptyDoc :: TeachQuery -> TeachingDoc
emptyDoc query = TeachingDoc
  { tdTitle = "Understanding: " <> tqTopic query
  , tdTopic = tqTopic query
  , tdCore = []
  , tdPrereqs = []
  , tdSupport = []
  }


-- ════════════════════════════════════════════════════════════════════════════
-- UTILITIES
-- ════════════════════════════════════════════════════════════════════════════

-- | Strip file:// prefix from URI.
stripFilePrefix :: Text -> Text
stripFilePrefix uri = fromMaybe uri $ T.stripPrefix "file://" uri
