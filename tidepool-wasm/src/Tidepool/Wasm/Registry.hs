{-# LANGUAGE OverloadedStrings #-}

-- | Unified Graph Registry - Single source of truth for all graphs.
--
-- This module provides:
--
-- 1. 'ActiveSession' - Runtime state for the currently executing graph
-- 2. 'GraphEntry' - How to create sessions for a graph
-- 3. 'graphRegistry' - Map from graphId to GraphEntry
-- 4. Unified FFI: 4 exports that take graphId as parameter
--
-- = Adding a New Graph
--
-- 1. Create your graph module (e.g., NewGraph.hs)
-- 2. Create a registry entry in Registry/NewGraph.hs
-- 3. Add entry to 'graphRegistry' in this file
-- 4. Regenerate TypeScript: @just regenerate@
--
-- That's it! No more editing Ffi.hs, cabal linker flags, or GraphSpecs.hs.
module Tidepool.Wasm.Registry
  ( -- * Types (re-exported from Registry.Types)
    ActiveSession(..)
  , GraphEntry(..)

    -- * Registry
  , graphRegistry
  , graphIds

    -- * Graph Specs (for codegen)
  , registryGraphSpecs

    -- * Unified FFI (Text interface for native testing)
  , initialize
  , step
  , getGraphInfo
  , getGraphState

    -- * Testing
  , resetSession
  ) where

import Data.Aeson (Value(..), encode, eitherDecodeStrict)
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector as V
import System.IO.Unsafe (unsafePerformIO)

import Tidepool.Wasm.WireTypes
  ( ExecutionPhase(..)
  , GraphState(..)
  , StepOutput(..)
  )
import Tidepool.Wasm.Ffi.Types (encodeStepOutput, mkErrorOutput)
import Tidepool.Generated.Codegen (GraphSpec(..))

-- Types and graph entries
import Tidepool.Wasm.Registry.Types (ActiveSession(..), GraphEntry(..))
import Tidepool.Wasm.Registry.TestGraph (testGraphEntry)
import Tidepool.Wasm.Registry.ExampleGraph (exampleGraphEntry)
import Tidepool.Wasm.Registry.HabiticaGraph (habiticaGraphEntry)


-- ════════════════════════════════════════════════════════════════════════════
-- REGISTRY
-- ════════════════════════════════════════════════════════════════════════════

-- | The graph registry - single source of truth.
--
-- To add a new graph: add one line here, regenerate TypeScript.
graphRegistry :: Map Text GraphEntry
graphRegistry = Map.fromList
  [ ("test", testGraphEntry)
  , ("example", exampleGraphEntry)
  , ("habitica", habiticaGraphEntry)
  ]

-- | List of valid graph IDs (for error messages).
graphIds :: [Text]
graphIds = Map.keys graphRegistry


-- | Derive GraphSpecs from registry entries (for codegen).
--
-- This is the SINGLE SOURCE OF TRUTH for graph metadata.
-- GraphSpecs.hs should import and re-export this.
registryGraphSpecs :: [GraphSpec]
registryGraphSpecs = map entryToSpec $ Map.toList graphRegistry
  where
    entryToSpec :: (Text, GraphEntry) -> GraphSpec
    entryToSpec (gid, entry) = GraphSpec
      { gsId = gid
      , gsName = entry.geName
      , gsNodes = extractNodes entry.geGraphInfo
      , gsEdges = extractEdges entry.geGraphInfo
      }

    -- Extract nodes array from JSON: {"nodes": ["entry", "compute", "exit"]}
    extractNodes :: Value -> [Text]
    extractNodes (Object o) = case KM.lookup "nodes" o of
      Just (Array arr) -> [t | String t <- V.toList arr]
      _ -> []
    extractNodes _ = []

    -- Extract edges from JSON: {"edges": [{"from": "a", "to": "b"}]}
    extractEdges :: Value -> [(Text, Text)]
    extractEdges (Object o) = case KM.lookup "edges" o of
      Just (Array arr) -> concatMap parseEdge (V.toList arr)
      _ -> []
    extractEdges _ = []

    parseEdge :: Value -> [(Text, Text)]
    parseEdge (Object o) = case (KM.lookup "from" o, KM.lookup "to" o) of
      (Just (String f), Just (String t)) -> [(f, t)]
      _ -> []
    parseEdge _ = []


-- ════════════════════════════════════════════════════════════════════════════
-- ACTIVE SESSION STATE
-- ════════════════════════════════════════════════════════════════════════════

-- | Global active session. Safe in WASM (one instance per Durable Object).
{-# NOINLINE activeSession #-}
activeSession :: IORef (Maybe ActiveSession)
activeSession = unsafePerformIO $ newIORef Nothing

-- | Reset session (for testing).
resetSession :: IO ()
resetSession = writeIORef activeSession Nothing


-- ════════════════════════════════════════════════════════════════════════════
-- UNIFIED FFI
-- ════════════════════════════════════════════════════════════════════════════

-- | Initialize a graph session.
--
-- @initialize "habitica" "{\"messageText\": \"done my workout\"}"@
--
-- Clears any previous session (atomic), looks up graph in registry,
-- creates new session, returns initial StepOutput.
initialize :: Text -> Text -> IO Text
initialize graphId inputJson = do
  -- Always clear previous session (atomic, prevents state leakage)
  writeIORef activeSession Nothing

  case Map.lookup graphId graphRegistry of
    Nothing -> pure $ encodeStepOutput $ mkErrorOutput $
      "Unknown graph: " <> graphId <> ". Valid graphs: " <> T.intercalate ", " graphIds

    Just entry -> do
      case eitherDecodeStrict (encodeUtf8 inputJson) of
        Left err -> pure $ encodeStepOutput $ mkErrorOutput $
          "JSON parse error: " <> T.pack err

        Right inputValue -> do
          result <- entry.geCreate inputValue
          case result of
            Left err -> pure $ encodeStepOutput $ mkErrorOutput err
            Right (output, session) -> do
              writeIORef activeSession (Just session)
              pure $ encodeStepOutput output


-- | Step the current session with an effect result.
--
-- @step "habitica" "{\"type\": \"success\", \"value\": {...}}"@
--
-- Validates graphId matches active session (catches routing bugs),
-- then calls the stored step function.
step :: Text -> Text -> IO Text
step graphId resultJson = do
  mSession <- readIORef activeSession
  case mSession of
    Nothing -> pure $ encodeStepOutput $ mkErrorOutput $
      "No active session - call initialize(\"" <> graphId <> "\", ...) first"

    Just session
      | session.asGraphId /= graphId -> pure $ encodeStepOutput $ mkErrorOutput $
          "Graph mismatch: session active for '" <> session.asGraphId <>
          "' but step called with '" <> graphId <>
          "'. This usually indicates a routing bug in the TypeScript harness."

      | otherwise -> do
          case eitherDecodeStrict (encodeUtf8 resultJson) of
            Left err -> pure $ encodeStepOutput $ mkErrorOutput $
              "JSON parse error: " <> T.pack err
            Right effectResult -> do
              output <- session.asStep effectResult
              -- Check if we're done (clear session on completion)
              case output of
                StepDone{} -> writeIORef activeSession Nothing
                StepFailed{} -> writeIORef activeSession Nothing
                StepYield{} -> pure ()  -- Keep session active
              pure $ encodeStepOutput output


-- | Get graph info for a graph ID.
--
-- @getGraphInfo "habitica"@
--
-- Returns static graph metadata (nodes, edges). Does NOT require
-- an active session - can be called anytime.
getGraphInfo :: Text -> IO Text
getGraphInfo graphId =
  case Map.lookup graphId graphRegistry of
    Nothing -> pure $ encodeStepOutput $ mkErrorOutput $
      "Unknown graph: " <> graphId <> ". Valid graphs: " <> T.intercalate ", " graphIds
    Just entry ->
      pure $ TL.toStrict $ TLE.decodeUtf8 $ encode entry.geGraphInfo


-- | Get current graph state.
--
-- @getGraphState "habitica"@
--
-- Returns runtime state (phase, completed nodes). Requires matching
-- active session.
getGraphState :: Text -> IO Text
getGraphState graphId = do
  mSession <- readIORef activeSession
  case mSession of
    Nothing ->
      pure $ TL.toStrict $ TLE.decodeUtf8 $ encode $
        GraphState PhaseIdle []

    Just session
      | session.asGraphId /= graphId ->
          pure $ TL.toStrict $ TLE.decodeUtf8 $ encode $
            GraphState PhaseIdle []

      | otherwise -> do
          gs <- session.asGetState
          pure $ TL.toStrict $ TLE.decodeUtf8 $ encode gs
