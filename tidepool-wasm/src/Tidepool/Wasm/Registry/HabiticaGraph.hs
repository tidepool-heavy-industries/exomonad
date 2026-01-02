{-# LANGUAGE OverloadedStrings #-}

-- | HabiticaRoutingGraph registry entry.
--
-- Uses per-session IORef pattern: each createSession allocates a fresh IORef
-- that lives only as long as the session. This avoids state leakage between
-- graphs when switching sessions.
module Tidepool.Wasm.Registry.HabiticaGraph
  ( habiticaGraphEntry
  ) where

import Data.Aeson (ToJSON(..), Value(..), Result(..), object, (.=), fromJSON)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Tidepool.Wasm.WireTypes
  ( EffectResult(..)
  , ExecutionPhase(..)
  , GraphState(..)
  , StepOutput(..)
  )
import Tidepool.Wasm.Runner (WasmResult(..), initializeWasm)
import Tidepool.Wasm.Ffi.Types (mkErrorOutput)
import Tidepool.Wasm.HabiticaRoutingGraph (ExecutionResult(..), runHabiticaRoutingGraph)
import Tidepool.Wasm.HabiticaTypes (RawInput(..))
import Tidepool.Wasm.Registry.Types (GraphEntry(..), ActiveSession(..))


-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH ENTRY
-- ════════════════════════════════════════════════════════════════════════════

habiticaGraphEntry :: GraphEntry
habiticaGraphEntry = GraphEntry
  { geName = "HabiticaRoutingGraph"
  , geGraphInfo = graphInfo
  , geCreate = createSession
  }


-- ════════════════════════════════════════════════════════════════════════════
-- STATIC METADATA
-- ════════════════════════════════════════════════════════════════════════════

graphInfo :: Value
graphInfo = object
  [ "name" .= ("HabiticaRoutingGraph" :: Text)
  , "id" .= ("habitica" :: Text)
  , "nodes" .= (["entry", "extractTask", "fetchExisting", "matchTask", "suggestAction", "confirmWithUser", "executeAction", "exit"] :: [Text])
  , "edges" .=
      [ object ["from" .= ("entry" :: Text), "to" .= ("extractTask" :: Text)]
      , object ["from" .= ("extractTask" :: Text), "to" .= ("fetchExisting" :: Text)]
      , object ["from" .= ("fetchExisting" :: Text), "to" .= ("matchTask" :: Text)]
      , object ["from" .= ("matchTask" :: Text), "to" .= ("suggestAction" :: Text)]
      , object ["from" .= ("suggestAction" :: Text), "to" .= ("confirmWithUser" :: Text)]
      , object ["from" .= ("confirmWithUser" :: Text), "to" .= ("executeAction" :: Text)]
      , object ["from" .= ("confirmWithUser" :: Text), "to" .= ("suggestAction" :: Text)]
      , object ["from" .= ("confirmWithUser" :: Text), "to" .= ("exit" :: Text)]
      , object ["from" .= ("executeAction" :: Text), "to" .= ("exit" :: Text)]
      ]
  ]


-- ════════════════════════════════════════════════════════════════════════════
-- SESSION STATE (per-session, not global)
-- ════════════════════════════════════════════════════════════════════════════

-- | Internal state for a single session.
-- Each session gets its own IORef - no global state.
data HabiticaState
  = HabiticaIdle
  | HabiticaWaiting (EffectResult -> WasmResult ExecutionResult) ExecutionPhase


-- ════════════════════════════════════════════════════════════════════════════
-- SESSION CREATION
-- ════════════════════════════════════════════════════════════════════════════

exitToOutput :: ExecutionResult -> StepOutput
exitToOutput result =
  let resultVal = toJSON result
  in StepDone resultVal (GraphState (PhaseCompleted resultVal) ["executeAction"])


-- | Create a new session for HabiticaRoutingGraph.
--
-- Each call allocates a fresh IORef for this session's state.
-- When the session is replaced in Registry.activeSession, this IORef
-- becomes unreachable and can be garbage collected.
createSession :: Value -> IO (Either Text (StepOutput, ActiveSession))
createSession inputValue = do
  -- Allocate fresh IORef for THIS session (not global!)
  sessionState <- newIORef HabiticaIdle

  case fromJSON inputValue of
    Error err -> pure $ Left $ "Invalid input for HabiticaRoutingGraph: " <> T.pack err
    Success (input :: RawInput) -> do
      let result = initializeWasm (runHabiticaRoutingGraph input)
      output <- wasmResultToOutput sessionState result
      let session = ActiveSession
            { asGraphId = "habitica"
            , asStep = stepSession sessionState
            , asGetState = getSessionState sessionState
            , asGraphInfo = graphInfo
            }
      pure $ Right (output, session)


-- | Step the session (captures sessionState IORef in closure)
stepSession :: IORef HabiticaState -> EffectResult -> IO StepOutput
stepSession sessionState effectResult = do
  st <- readIORef sessionState
  case st of
    HabiticaIdle -> pure $ mkErrorOutput "HabiticaRoutingGraph not initialized"
    HabiticaWaiting resume _phase -> do
      let result = resume effectResult
      wasmResultToOutput sessionState result


-- | Get current state (captures sessionState IORef in closure)
getSessionState :: IORef HabiticaState -> IO GraphState
getSessionState sessionState = do
  st <- readIORef sessionState
  pure $ case st of
    HabiticaIdle -> GraphState PhaseIdle []
    HabiticaWaiting _ phase -> GraphState phase []


-- | Convert WasmResult to StepOutput and update session state
wasmResultToOutput :: IORef HabiticaState -> WasmResult ExecutionResult -> IO StepOutput
wasmResultToOutput sessionState (WasmComplete a) = do
  writeIORef sessionState HabiticaIdle
  pure $ exitToOutput a

wasmResultToOutput sessionState (WasmYield eff resume) = do
  let phase = PhaseInNode "extractTask"
  writeIORef sessionState (HabiticaWaiting resume phase)
  pure $ StepYield eff (GraphState phase [])

wasmResultToOutput sessionState (WasmError msg) = do
  writeIORef sessionState HabiticaIdle
  pure $ mkErrorOutput msg
