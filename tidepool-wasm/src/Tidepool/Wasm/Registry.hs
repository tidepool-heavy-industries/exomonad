{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Unified Graph Registry - configurable at init time.
--
-- This module provides:
--
-- 1. 'ActiveSession' - Runtime state for the currently executing graph
-- 2. 'GraphEntry' - How to create sessions for a graph
-- 3. 'setRegistry' - Configure which graphs are available
-- 4. 'makeGraphEntry' - One-liner to create GraphEntry from graph type
-- 5. Unified FFI: 4 exports that take graphId as parameter
--
-- = Usage (defining a graph entry)
--
-- @
-- myGraphEntry :: GraphEntry
-- myGraphEntry = makeGraphEntry \@MyGraph "mygraph" runMyGraph
-- @
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

    -- * Graph Entry Helper
  , makeGraphEntry
  , graphInfoToJson

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

import Data.Aeson (Value(..), FromJSON, ToJSON(..), encode, eitherDecodeStrict, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector as V
import GHC.Generics (Generic, Rep)
import System.IO.Unsafe (unsafePerformIO)

import Tidepool.Graph.Reify
  ( GraphInfo(..)
  , NodeInfo(..)
  , EdgeInfo(..)
  , GReifyFields
  , ReifyMaybeType
  , GetEntryTypeFromGraph
  , GetExitTypeFromGraph
  , makeGraphInfo
  )
import Tidepool.Graph.Generic (AsGraph)
import Tidepool.Wasm.Runner (WasmResult(..), initializeWasm, WasmM)
import Tidepool.Wasm.WireTypes
  ( EffectResult
  , ExecutionPhase(..)
  , GraphState(..)
  , StepOutput(..)
  )
import Tidepool.Wasm.Ffi.Types (encodeStepOutput, mkErrorOutput)
import Tidepool.Generated.Codegen (GraphSpec(..))

-- Types
import Tidepool.Wasm.Registry.Types (ActiveSession(..), GraphEntry(..))


-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH ENTRY HELPER
-- ════════════════════════════════════════════════════════════════════════════

-- | Create a GraphEntry from a graph type and runner.
--
-- This is the primary API for defining graph entries. It:
--
-- * Derives graphInfo (nodes, edges) from the graph type via Generics
-- * Uses the graph ID as single source of truth (no duplication)
-- * Handles all session state boilerplate
--
-- @
-- testGraphEntry :: GraphEntry
-- testGraphEntry = makeGraphEntry \@TestGraph "test" computeHandlerWasm
-- @
makeGraphEntry
  :: forall graph input output.
     ( Generic (graph AsGraph)
     , GReifyFields (Rep (graph AsGraph))
     , ReifyMaybeType (GetEntryTypeFromGraph graph)
     , ReifyMaybeType (GetExitTypeFromGraph graph)
     , FromJSON input
     , ToJSON output
     )
  => Text                    -- ^ Graph ID (single source of truth)
  -> (input -> WasmM output) -- ^ Graph runner
  -> GraphEntry
makeGraphEntry graphId runner = GraphEntry
  { geName = graphId
  , geGraphInfo = graphInfoToJson graphId (makeGraphInfo (Proxy @graph))
  , geCreate = createSession graphId runner
  }


-- | Convert GraphInfo to JSON Value for FFI.
--
-- Produces JSON in the format expected by TypeScript:
--
-- @
-- { "id": "test",
--   "name": "test",
--   "nodes": ["entry", "compute", "exit"],
--   "edges": [{"from": "entry", "to": "compute"}, ...]
-- }
-- @
graphInfoToJson :: Text -> GraphInfo -> Value
graphInfoToJson graphId gi = object
  [ "id" .= graphId
  , "name" .= graphId
  , "nodes" .= nodeNames
  , "edges" .= map edgeToJson (gi.giEdges)
  ]
  where
    -- Include entry/exit plus all real nodes
    nodeNames :: [Text]
    nodeNames = "entry" : map (.niName) (gi.giNodes) ++ ["exit"]

    edgeToJson :: EdgeInfo -> Value
    edgeToJson e = object
      [ "from" .= e.eiFrom
      , "to" .= e.eiTo
      ]


-- ════════════════════════════════════════════════════════════════════════════
-- SESSION STATE (generic, used by makeGraphEntry)
-- ════════════════════════════════════════════════════════════════════════════

-- | Generic session state, parameterized by output type.
data SessionState a
  = SessionIdle
  | SessionWaiting (EffectResult -> WasmResult a) ExecutionPhase


-- | Create a new session for a graph.
createSession
  :: forall input output.
     (FromJSON input, ToJSON output)
  => Text                      -- ^ Graph ID
  -> (input -> WasmM output)   -- ^ Runner
  -> Value                     -- ^ Input JSON
  -> IO (Either Text (StepOutput, ActiveSession))
createSession graphId runner inputValue = do
  sessionState <- newIORef SessionIdle

  case Aeson.fromJSON inputValue of
    Aeson.Error err -> pure $ Left $ "Invalid input: " <> T.pack err
    Aeson.Success input -> do
      let result = initializeWasm (runner input)
      output <- wasmResultToOutput sessionState result
      let session = ActiveSession
            { asGraphId = graphId
            , asStep = stepSession sessionState
            , asGetState = getSessionState sessionState
            , asGraphInfo = object ["id" .= graphId]  -- Minimal, full info in GraphEntry
            }
      pure $ Right (output, session)


-- | Step the session with an effect result.
stepSession :: ToJSON a => IORef (SessionState a) -> EffectResult -> IO StepOutput
stepSession sessionState effectResult = do
  st <- readIORef sessionState
  case st of
    SessionIdle -> pure $ mkErrorOutput "Session not initialized"
    SessionWaiting resume _phase -> do
      let result = resume effectResult
      wasmResultToOutput sessionState result


-- | Get current session state.
getSessionState :: IORef (SessionState a) -> IO GraphState
getSessionState sessionState = do
  st <- readIORef sessionState
  pure $ case st of
    SessionIdle -> GraphState PhaseIdle []
    SessionWaiting _ phase -> GraphState phase []


-- | Convert WasmResult to StepOutput.
wasmResultToOutput :: ToJSON a => IORef (SessionState a) -> WasmResult a -> IO StepOutput
wasmResultToOutput sessionState (WasmComplete result) = do
  writeIORef sessionState SessionIdle
  let resultVal = toJSON result
  pure $ StepDone resultVal (GraphState (PhaseCompleted resultVal) [])

wasmResultToOutput sessionState (WasmYield eff resume) = do
  let phase = PhaseInNode "running"  -- Generic phase name
  writeIORef sessionState (SessionWaiting resume phase)
  pure $ StepYield eff (GraphState phase [])

wasmResultToOutput sessionState (WasmError msg) = do
  writeIORef sessionState SessionIdle
  pure $ mkErrorOutput msg


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
