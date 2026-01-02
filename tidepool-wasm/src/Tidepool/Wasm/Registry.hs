{-# LANGUAGE OverloadedStrings #-}

-- | Unified Graph Registry - configurable at init time.
--
-- This module provides:
--
-- 1. 'ActiveSession' - Runtime state for the currently executing graph
-- 2. 'GraphEntry' - How to create sessions for a graph
-- 3. 'setRegistry' - Configure which graphs are available
-- 4. Unified FFI: 4 exports that take graphId as parameter
--
-- = Usage (in your reactor)
--
-- @
-- import Tidepool.Wasm.Registry (setRegistry)
-- import MyApp.Graphs (myGraphEntry)
--
-- initRegistry :: IO ()
-- initRegistry = setRegistry [("mygraph", myGraphEntry)]
-- @
--
-- Call 'setRegistry' once at init before any FFI calls.
module Tidepool.Wasm.Registry
  ( -- * Types (re-exported from Registry.Types)
    ActiveSession(..)
  , GraphEntry(..)

    -- * Registry Configuration
  , setRegistry
  , getRegistry

    -- * Graph Specs (for codegen)
  , registryGraphSpecs

    -- * Unified FFI (Text interface for native testing)
  , initialize
  , step
  , getGraphInfo
  , getGraphState

    -- * Testing
  , resetSession
  , resetRegistry
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

-- Types
import Tidepool.Wasm.Registry.Types (ActiveSession(..), GraphEntry(..))


-- ════════════════════════════════════════════════════════════════════════════
-- REGISTRY
-- ════════════════════════════════════════════════════════════════════════════

-- | Global registry IORef. Set by application via 'setRegistry'.
{-# NOINLINE registryRef #-}
registryRef :: IORef (Map Text GraphEntry)
registryRef = unsafePerformIO $ newIORef Map.empty

-- | Set the graph registry. Call once at init before any FFI calls.
--
-- @
-- setRegistry [("habitica", habiticaGraphEntry), ("test", testGraphEntry)]
-- @
setRegistry :: [(Text, GraphEntry)] -> IO ()
setRegistry entries = writeIORef registryRef (Map.fromList entries)

-- | Get current registry (for FFI functions).
getRegistry :: IO (Map Text GraphEntry)
getRegistry = readIORef registryRef

-- | Reset registry (for testing).
resetRegistry :: IO ()
resetRegistry = writeIORef registryRef Map.empty

-- | Get list of valid graph IDs (for error messages).
graphIds :: IO [Text]
graphIds = Map.keys <$> getRegistry


-- | Derive GraphSpecs from registry entries (for codegen).
--
-- This is the SINGLE SOURCE OF TRUTH for graph metadata.
-- GraphSpecs.hs should import and re-export this.
registryGraphSpecs :: IO [GraphSpec]
registryGraphSpecs = map entryToSpec . Map.toList <$> getRegistry
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

  registry <- getRegistry
  validIds <- graphIds
  case Map.lookup graphId registry of
    Nothing -> pure $ encodeStepOutput $ mkErrorOutput $
      "Unknown graph: " <> graphId <> ". Valid graphs: " <> T.intercalate ", " validIds

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
getGraphInfo graphId = do
  registry <- getRegistry
  validIds <- graphIds
  case Map.lookup graphId registry of
    Nothing -> pure $ encodeStepOutput $ mkErrorOutput $
      "Unknown graph: " <> graphId <> ". Valid graphs: " <> T.intercalate ", " validIds
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
