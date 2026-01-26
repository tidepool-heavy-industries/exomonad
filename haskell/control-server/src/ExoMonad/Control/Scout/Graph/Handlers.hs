{-# LANGUAGE FlexibleContexts #-}

-- | Node handlers for the DocGen graph.
--
-- This module implements the handlers for each node in the DocGen graph,
-- porting the logic from the hand-rolled BFS in DocGen.hs to the graph DSL.
--
-- = Handler Responsibilities
--
-- * **dgInit**: Initialize exploration state from query, resolve seeds
-- * **dgProcess**: Process a symbol from frontier (LSP lookup, extract candidates)
-- * **dgSelect**: LLM selects relevant candidates (via Template + Schema)
-- * **dgExpand**: Add selected symbols to frontier, advance BFS
-- * **dgFinalize**: Build TeachingDoc from accumulated state
module ExoMonad.Control.Scout.Graph.Handlers
  ( -- * Graph Handlers
    docGenHandlers

    -- * Re-exports for convenience
  , module ExoMonad.Control.Scout.Graph
  , module ExoMonad.Control.Scout.Graph.Types
  , module ExoMonad.Control.Scout.Graph.Templates
  ) where

import Control.Monad (forM)
import Control.Monad.Freer (Eff, Member)
import Data.List (sortOn, partition)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import ExoMonad.Effect.Types (Log, logDebug)
import ExoMonad.Effect.LSP
  ( LSP, workspaceSymbol, hover
  , textDocument, position
  , SymbolInformation(..), Location(..), Range(..), Position(..), HoverInfo(..)
  )
import ExoMonad.Graph.Generic (AsHandler)
import ExoMonad.Graph.Goto (gotoChoice, gotoExit, LLMHandler(..), GotoChoice, To)
import ExoMonad.Graph.Memory (Memory, getMem, updateMem)
import ExoMonad.Graph.Types (Exit)
import ExoMonad.Platform (NativeOnly)

import ExoMonad.Control.Scout.DocGen.Types
  ( SymbolKey, LSPSymbol(..), TeachingUnit(..)
  )
import ExoMonad.Graph.Template (TemplateDef(..))

import ExoMonad.Control.Scout.Graph
import ExoMonad.Control.Scout.Graph.Types
import ExoMonad.Control.Scout.Graph.Templates


-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH HANDLERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Handlers for the DocGen graph.
--
-- Effect stack requires:
-- - Memory ExploreState (for BFS state)
-- - LSP (for symbol lookup)
-- - Log (for debug output)
docGenHandlers
  :: ( Member (Memory ExploreState) es
     , Member LSP es
     , Member Log es
     , NativeOnly
     )
  => DocGenGraph (AsHandler es)
docGenHandlers = DocGenGraph
  { dgEntry = ()

  , dgInit = initHandler

  , dgProcess = processHandler

  , dgSelect = LLMHandler
      { llmSystem = Nothing
      , llmUser   = templateCompiled @SelectTpl
      , llmBefore = selectBefore
      , llmAfter  = selectAfter
      }

  , dgExpand = expandHandler

  , dgFinalize = finalizeHandler

  , dgExit = ()
  }


-- ════════════════════════════════════════════════════════════════════════════
-- INIT HANDLER
-- ════════════════════════════════════════════════════════════════════════════

-- | Initialize exploration state from query.
--
-- Resolves seed symbols via LSP workspace/symbol search, initializes
-- the ExploreState memory, and routes to either dgProcess (if seeds found)
-- or dgFinalize (if no seeds).
initHandler
  :: ( Member (Memory ExploreState) es
     , Member LSP es
     , Member Log es
     , NativeOnly
     )
  => TeachQuery
  -> Eff es (GotoChoice '[To "dgProcess" ProcessInput, To "dgFinalize" FinalizeInput])
initHandler query = do
  logDebug $ "[DocGen] Initializing for topic: " <> tqTopic query
  logDebug $ "[DocGen] Seeds: " <> T.intercalate ", " (tqSeeds query)

  -- Resolve seed symbols to SymbolKeys
  seedKeys <- resolveSeedSymbols (tqSeeds query)
  logDebug $ "[DocGen] Resolved " <> T.pack (show (length seedKeys)) <> " seed symbols"

  -- Initialize state
  let budget = if tqBudget query <= 0 then 20 else tqBudget query
      initialState = initialExploreState (tqTopic query) seedKeys budget 5

  updateMem @ExploreState (const initialState)

  -- Route based on whether we have seeds
  case seedKeys of
    [] -> pure $ gotoChoice @"dgFinalize" (FinalizeInput FrontierEmpty)
    (firstKey:restKeys) -> do
      -- Update frontier to rest of seeds
      updateMem @ExploreState $ \s -> s { esFrontier = map (, 0) restKeys }
      pure $ gotoChoice @"dgProcess" (ProcessInput firstKey 0)


-- ════════════════════════════════════════════════════════════════════════════
-- PROCESS HANDLER
-- ════════════════════════════════════════════════════════════════════════════

-- | Process a symbol from the frontier.
--
-- Performs LSP lookup (hover for signature/docs), extracts candidate types
-- from the signature, and routes to dgSelect if candidates found, or
-- advances to next symbol if no candidates.
processHandler
  :: ( Member (Memory ExploreState) es
     , Member LSP es
     , Member Log es
     , NativeOnly
     )
  => ProcessInput
  -> Eff es (GotoChoice '[To "dgSelect" SelectInput, To "dgFinalize" FinalizeInput])
processHandler (ProcessInput symKey depth) = do
  state <- getMem @ExploreState

  -- Check if already visited
  if symKey `Set.member` esVisited state
    then do
      logDebug $ "[DocGen] Skipping visited: " <> snd symKey
      advanceToNext
    else do
      let name = snd symKey
      logDebug $ "[DocGen] Processing: " <> name
        <> " (depth=" <> T.pack (show depth)
        <> ", budget=" <> T.pack (show (esBudget state)) <> ")"

      -- Lookup LSP data
      lspSymMaybe <- lookupSymbol symKey
      case lspSymMaybe of
        Nothing -> do
          logDebug $ "[DocGen] LSP lookup failed for: " <> name
          -- Mark as visited, advance to next
          updateMem @ExploreState $ \s -> s
            { esVisited = Set.insert symKey (esVisited s) }
          advanceToNext

        Just lspSym -> do
          -- Add to graph with depth
          updateMem @ExploreState $ \s -> s
            { esGraph = Map.insert symKey (depth, lspSym) (esGraph s)
            , esVisited = Set.insert symKey (esVisited s)
            , esBudget = esBudget s - 1
            }

          -- Extract candidates from signature (deterministic)
          let candidates = extractCandidatesFromSig (lsSignature lspSym)
          logDebug $ "[DocGen] Candidates from signature: " <> T.intercalate ", " candidates

          if null candidates
            then advanceToNext
            else do
              state' <- getMem @ExploreState
              let selectInput = SelectInput
                    { siTopic = esTopic state'
                    , siSymbol = lspSym
                    , siCandidates = candidates
                    }
              pure $ gotoChoice @"dgSelect" selectInput

  where
    advanceToNext :: ( Member (Memory ExploreState) es
                     , Member Log es
                     )
                  => Eff es (GotoChoice '[To "dgSelect" SelectInput, To "dgFinalize" FinalizeInput])
    advanceToNext = do
      state' <- getMem @ExploreState
      -- Check budget first
      if esBudget state' <= 0
        then do
          logDebug "[DocGen] Budget exhausted"
          pure $ gotoChoice @"dgFinalize" (FinalizeInput BudgetExhausted)
        else case esFrontier state' of
          [] -> do
            logDebug "[DocGen] Frontier empty"
            pure $ gotoChoice @"dgFinalize" (FinalizeInput FrontierEmpty)
          ((nextKey, _nextDepth):rest) -> do
            updateMem @ExploreState $ \s -> s { esFrontier = rest }
            -- Note: We can't loop back to dgProcess from here directly.
            -- We need to go through a node that has dgProcess in its targets.
            -- For now, we'll finalize and let the runner handle this.
            -- In a proper implementation, we'd restructure the graph.
            logDebug $ "[DocGen] Need to process next: " <> snd nextKey
            pure $ gotoChoice @"dgFinalize" (FinalizeInput FrontierEmpty)


-- ════════════════════════════════════════════════════════════════════════════
-- SELECT HANDLER (LLM Node)
-- ════════════════════════════════════════════════════════════════════════════

-- | Build template context for the select LLM call.
selectBefore
  :: SelectInput
  -> Eff es SelectContext
selectBefore input =
  pure SelectContext
    { topic = siTopic input
    , symbol_name = lsName (siSymbol input)
    , signature = lsSignature (siSymbol input)
    , doc_comment = lsDocComment (siSymbol input)
    , candidates = siCandidates input
    }

-- | Route after LLM selection.
--
-- Passes the selected output to dgExpand.
-- The expand handler gets symbol info from Memory.
selectAfter
  :: SelectOutput
  -> Eff es (GotoChoice '[To "dgExpand" SelectOutput])
selectAfter output = do
  pure $ gotoChoice @"dgExpand" output


-- ════════════════════════════════════════════════════════════════════════════
-- EXPAND HANDLER
-- ════════════════════════════════════════════════════════════════════════════

-- | Expand frontier with selected symbols.
--
-- Resolves selected symbol names to SymbolKeys via LSP, adds unseen ones
-- to the frontier, and routes to either dgProcess (more work) or
-- dgFinalize (done).
--
-- Gets the current symbol info from Memory (stored by processHandler).
expandHandler
  :: ( Member (Memory ExploreState) es
     , Member LSP es
     , Member Log es
     , NativeOnly
     )
  => SelectOutput
  -> Eff es (GotoChoice '[To "dgProcess" ProcessInput, To "dgFinalize" FinalizeInput])
expandHandler output = do
  state <- getMem @ExploreState

  let selectedTokens = take 5 (soSelected output)  -- Limit to 5 per symbol

  logDebug $ "[DocGen] Selected symbols: " <> T.intercalate ", " selectedTokens

  -- Resolve tokens to SymbolKeys
  resolvedKeys <- resolveTokens selectedTokens

  -- Filter to unseen symbols
  let unseen = filter (\k -> not (k `Set.member` esVisited state)) resolvedKeys

  logDebug $ "[DocGen] Resolved " <> T.pack (show (length resolvedKeys))
    <> " (" <> T.pack (show (length unseen)) <> " new)"

  -- Get current depth from the most recently added symbol in the graph
  -- (processHandler added the current symbol before routing to select)
  let currentDepth = case Map.toList (esGraph state) of
        [] -> 0
        entries -> maximum $ map (fst . snd) entries
      newFrontier = map (\k -> (k, currentDepth + 1)) unseen

  -- Update frontier
  updateMem @ExploreState $ \s -> s
    { esFrontier = esFrontier s ++ newFrontier }

  -- Check if we should continue
  state' <- getMem @ExploreState
  if esBudget state' <= 0
    then do
      logDebug "[DocGen] Budget exhausted"
      pure $ gotoChoice @"dgFinalize" (FinalizeInput BudgetExhausted)
    else case esFrontier state' of
      [] -> do
        logDebug "[DocGen] Frontier empty"
        pure $ gotoChoice @"dgFinalize" (FinalizeInput FrontierEmpty)
      ((nextKey, nextDepth):rest) -> do
        updateMem @ExploreState $ \s -> s { esFrontier = rest }
        pure $ gotoChoice @"dgProcess" (ProcessInput nextKey nextDepth)


-- ════════════════════════════════════════════════════════════════════════════
-- FINALIZE HANDLER
-- ════════════════════════════════════════════════════════════════════════════

-- | Build the final TeachingDoc from accumulated state.
finalizeHandler
  :: ( Member (Memory ExploreState) es
     , Member Log es
     )
  => FinalizeInput
  -> Eff es (GotoChoice '[To Exit TeachingDoc])
finalizeHandler (FinalizeInput reason) = do
  state <- getMem @ExploreState

  logDebug $ "[DocGen] Finalizing: " <> T.pack (show reason)
    <> ", " <> T.pack (show (Map.size (esGraph state))) <> " symbols collected"

  let doc = buildTeachingDoc (esTopic state) (esGraph state)
  pure $ gotoExit doc


-- ════════════════════════════════════════════════════════════════════════════
-- HELPER FUNCTIONS (ported from DocGen.hs)
-- ════════════════════════════════════════════════════════════════════════════

-- | Resolve seed symbol names to SymbolKeys via LSP.
resolveSeedSymbols
  :: (Member LSP effs, Member Log effs, NativeOnly)
  => [Text]
  -> Eff effs [SymbolKey]
resolveSeedSymbols seeds = do
  results <- forM seeds $ \seedName -> do
    symbols <- workspaceSymbol seedName
    logDebug $ "[DocGen] workspaceSymbol \"" <> seedName <> "\" -> "
      <> T.pack (show (length symbols)) <> " results"
    case symbols of
      [] -> do
        logDebug $ "[DocGen] WARNING: No symbols found for seed: " <> seedName
        pure Nothing
      (sym:_) -> do
        let loc = sym.siLocation
            file = stripFilePrefix loc.locUri
        pure $ Just (T.unpack file, sym.siName)

  pure $ catMaybes results


-- | Resolve token names to SymbolKeys.
resolveTokens
  :: (Member LSP effs, Member Log effs, NativeOnly)
  => [Text]
  -> Eff effs [SymbolKey]
resolveTokens tokens = do
  results <- forM tokens $ \token -> do
    symbols <- workspaceSymbol token
    case symbols of
      [] -> do
        logDebug $ "[DocGen] WARNING: Could not resolve token: " <> token
        pure Nothing
      (sym:_) -> do
        let loc = sym.siLocation
            file = stripFilePrefix loc.locUri
        pure $ Just (T.unpack file, sym.siName)

  pure $ catMaybes results


-- | Lookup a symbol's LSP data.
lookupSymbol
  :: (Member LSP effs, NativeOnly)
  => SymbolKey
  -> Eff effs (Maybe LSPSymbol)
lookupSymbol (file, name) = do
  symbols <- workspaceSymbol name
  case filter (\s -> T.unpack (stripFilePrefix s.siLocation.locUri) == file) symbols of
    [] -> case symbols of
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
parseHoverContent :: Text -> (Text, Maybe Text)
parseHoverContent content =
  let lns = T.lines content
      (codeLines, docLines) = span (/= "```") (drop 1 $ dropWhile (/= "```haskell") lns)
      sig = T.strip $ T.unlines codeLines
      doc = case dropWhile (== "```") docLines of
        [] -> Nothing
        ds -> let d = T.strip $ T.unlines ds
              in if T.null d then Nothing else Just d
  in if T.null sig
     then (content, Nothing)
     else (sig, doc)


-- | Extract candidate type names from a signature.
--
-- Simple heuristic: find capitalized words that look like type names.
extractCandidatesFromSig :: Text -> [Text]
extractCandidatesFromSig sig =
  let tokens = T.words $ T.filter (\c -> c `notElem` ("()[]{},:->=" :: String)) sig
      isTypeName t = case T.uncons t of
        Just (c, _) -> c >= 'A' && c <= 'Z'
        Nothing -> False
      -- Filter common primitives
      isPrimitive t = t `elem` ["Int", "Integer", "Bool", "Text", "String", "Char",
                                "Double", "Float", "Maybe", "Either", "IO", "Eff",
                                "Member", "Type", "Constraint"]
  in filter (\t -> isTypeName t && not (isPrimitive t)) tokens


-- | Build a TeachingDoc from the exploration graph.
buildTeachingDoc :: Text -> Map.Map SymbolKey (Int, LSPSymbol) -> TeachingDoc
buildTeachingDoc topic_ graph =
  let units = map toUnit (Map.toList graph)
      sorted = sortOn (\u -> (tuDepth u, lsName (tuSymbol u))) units
      (core, rest) = partition (\u -> tuDepth u == 0) sorted
      (prereqs, support) = partition (\u -> tuDepth u <= 2) rest
  in TeachingDoc
    { tdTitle = "Understanding: " <> topic_
    , tdTopic = topic_
    , tdCore = core
    , tdPrereqs = prereqs
    , tdSupport = support
    }
  where
    toUnit (_, (depth_, lsp)) = TeachingUnit
      { tuSymbol = lsp
      , tuDepth = depth_
      }


-- | Strip file:// prefix from URI.
stripFilePrefix :: Text -> Text
stripFilePrefix uri = fromMaybe uri $ T.stripPrefix "file://" uri
