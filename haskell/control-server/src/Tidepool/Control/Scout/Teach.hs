{-# LANGUAGE FlexibleContexts #-}

-- | Teaching document generation via LSP + Gemma.
--
-- Given a topic and seed symbols, explores the codebase to generate
-- a teaching document that explains the topic in prerequisite order.
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
-- │  2. For each symbol in frontier:        │
-- │     a. Lookup LSP data (hover, location)│
-- │     b. Classify with Gemma              │
-- │     c. Add mentions to frontier         │
-- │  3. Topological sort by mentions        │
-- │  4. Group by role                       │
-- │                                         │
-- └─────────────────────────────────────────┘
--     │
--     ▼
-- TeachingDoc { prereqs, core, support }
-- @
--
-- Key insight: Gemma extracts edges (what symbols are needed to explain
-- this symbol). Haskell traverses the graph and sorts topologically.
module Tidepool.Control.Scout.Teach
  ( -- * Main Entry Point
    teach

    -- * Configuration
  , TeachConfig(..)
  , defaultTeachConfig

    -- * Re-exports
  , module Tidepool.Control.Scout.Teach.Types
  ) where

import Control.Monad (forM)
import Control.Monad.Freer (Eff, Member, LastMember)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Tidepool.Control.Scout.Teach.Types
import Tidepool.Control.Scout.Teach.Gemma (TeachGemma, classifySymbol)
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

-- | Configuration for teaching exploration.
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

-- | Generate a teaching document from a query.
--
-- This is the main entry point. It:
--   1. Resolves seed symbols via LSP workspace/symbol
--   2. Explores the graph using BFS with Gemma classification
--   3. Topologically sorts by mentions
--   4. Groups into prereqs, core, and support sections
teach
  :: ( Member LSP effs
     , Member TeachGemma effs
     , Member Log effs
     , LastMember IO effs
     , NativeOnly
     )
  => TeachConfig
  -> TeachQuery
  -> Eff effs TeachingDoc
teach config query = do
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

      -- 4. Build teaching document
      let doc = buildTeachingDoc query (tsGraph finalState)
      logDebug $ "[Teach] Generated doc with "
        <> T.pack (show (length (tdPrereqs doc))) <> " prereqs, "
        <> T.pack (show (length (tdCore doc))) <> " core, "
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

-- | The core exploration loop.
--
-- Processes symbols from frontier until budget exhausted or frontier empty.
-- Each iteration:
--   1. Pop symbol from frontier
--   2. Skip if already visited
--   3. Lookup LSP data (hover, location)
--   4. Classify with Gemma
--   5. Add mentions to frontier
exploreLoop
  :: ( Member LSP effs
     , Member TeachGemma effs
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
  | Set.null (tsFrontier state) = do
      logDebug "[Teach] Frontier empty"
      pure state
  -- Process next symbol
  | otherwise = do
      -- Pop from frontier (take minimum for deterministic order)
      let (symKey, frontier') = Set.deleteFindMin (tsFrontier state)

      -- Skip if already visited
      if symKey `Set.member` tsVisited state
        then do
          logDebug $ "[Teach] Skipping already visited: " <> snd symKey
          exploreLoop config query state { tsFrontier = frontier' }
        else do
          let (file, name) = symKey
          logDebug $ "[Teach] Processing: " <> name
            <> " in " <> T.pack file
            <> " (budget=" <> T.pack (show (tsBudget state)) <> ")"

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
              -- Check cache first
              teachOutput <- case Map.lookup symKey (tsCache state) of
                Just cached -> do
                  logDebug "[Teach] Using cached classification"
                  pure cached
                Nothing -> do
                  -- Classify with Gemma
                  classifySymbol query lspSym

              -- Filter unrelated symbols
              if toRole teachOutput == Unrelated
                then do
                  logDebug $ "[Teach] Skipping unrelated: " <> name
                  let state' = state
                        { tsFrontier = frontier'
                        , tsVisited = Set.insert symKey (tsVisited state)
                        , tsCache = Map.insert symKey teachOutput (tsCache state)
                        }
                  exploreLoop config query state'
                else do
                  -- Add to graph
                  let graph' = Map.insert symKey (teachOutput, lspSym) (tsGraph state)

                  -- Resolve mentions to SymbolKeys
                  let mentionsLimited = take (tcMaxMentionsPerSymbol config) (toMentions teachOutput)
                  mentionKeys <- resolveMentions file mentionsLimited

                  -- Filter to unseen mentions
                  let unseen = Set.difference
                        (Set.fromList mentionKeys)
                        (tsVisited state)

                  logDebug $ "[Teach] -> role=" <> symbolRoleToText (toRole teachOutput)
                    <> ", prereq=" <> T.pack (show (toIsPrereq teachOutput))
                    <> ", mentions=" <> T.pack (show (length mentionKeys))
                    <> " (" <> T.pack (show (Set.size unseen)) <> " new)"

                  let state' = state
                        { tsGraph = graph'
                        , tsFrontier = Set.union frontier' unseen
                        , tsVisited = Set.insert symKey (tsVisited state)
                        , tsBudget = tsBudget state - 1
                        , tsCache = Map.insert symKey teachOutput (tsCache state)
                        }

                  exploreLoop config query state'


-- | Resolve mention names to SymbolKeys.
--
-- Uses the current file as context for resolution.
resolveMentions
  :: (Member LSP effs, NativeOnly)
  => FilePath   -- ^ Context file
  -> [Text]     -- ^ Mention names
  -> Eff effs [SymbolKey]
resolveMentions _contextFile mentions = do
  results <- forM mentions $ \mention -> do
    symbols <- workspaceSymbol mention
    case symbols of
      [] -> pure Nothing
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

-- | Build a teaching document from the exploration graph.
buildTeachingDoc :: TeachQuery -> Map SymbolKey (TeachOutput, LSPSymbol) -> TeachingDoc
buildTeachingDoc query graph =
  let -- Convert to list and sort
      items = Map.toList graph
      units = map toUnit items

      -- Check if a unit is a prerequisite based on whether other items reference it
      isPrereqUnit u = tuRole u == CoreConcept ||
        any (\(_, (t, _)) -> toIsPrereq t && lsName (tuSymbol u) `elem` toMentions t) items

      -- Topological sort by mentions (prereqs first)
      sorted = topoSort units

      -- Group by role
      (prereqs, rest1) = span isPrereqUnit sorted
      (core, support) = partition' isCoreUnit rest1

  in TeachingDoc
    { tdTitle = "Understanding: " <> tqTopic query
    , tdTopic = tqTopic query
    , tdPrereqs = prereqs
    , tdCore = core
    , tdSupport = support
    }
  where
    toUnit (_, (teach', lsp)) = TeachingUnit
      { tuSymbol = lsp
      , tuRole = toRole teach'
      , tuMentions = toMentions teach'
      }

    isCoreUnit u = tuRole u == CoreConcept || tuRole u == Implementation

    partition' p xs = (filter p xs, filter (not . p) xs)


-- | Topological sort by mentions.
--
-- Symbols that are mentioned by others come first.
topoSort :: [TeachingUnit] -> [TeachingUnit]
topoSort units =
  let -- Count how many times each symbol is mentioned
      mentionCounts = foldr countMentions Map.empty units
      -- Sort by mention count (descending) then name
  in sortOn (\u -> (negate $ Map.findWithDefault 0 (lsName (tuSymbol u)) mentionCounts, lsName (tuSymbol u))) units
  where
    countMentions u acc = foldr (\m -> Map.insertWith (+) m (1 :: Int)) acc (tuMentions u)


-- | Create an empty document when no seeds found.
emptyDoc :: TeachQuery -> TeachingDoc
emptyDoc query = TeachingDoc
  { tdTitle = "Understanding: " <> tqTopic query
  , tdTopic = tqTopic query
  , tdPrereqs = []
  , tdCore = []
  , tdSupport = []
  }


-- ════════════════════════════════════════════════════════════════════════════
-- UTILITIES
-- ════════════════════════════════════════════════════════════════════════════

-- | Strip file:// prefix from URI.
stripFilePrefix :: Text -> Text
stripFilePrefix uri = fromMaybe uri $ T.stripPrefix "file://" uri
