{-# LANGUAGE OverloadedStrings #-}

-- | TestGraph registry entry.
--
-- Uses per-session IORef pattern: each createSession allocates a fresh IORef
-- that lives only as long as the session. This avoids state leakage between
-- graphs when switching sessions.
module Tidepool.Wasm.Registry.TestGraph
  ( testGraphEntry
  ) where

import Data.Aeson (ToJSON(..), Value(..), object, (.=))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import Tidepool.Graph.Goto (GotoChoice(..), OneOf(..), To)
import Tidepool.Graph.Types (Exit)
import Tidepool.Wasm.WireTypes
  ( EffectResult(..)
  , ExecutionPhase(..)
  , GraphState(..)
  , StepOutput(..)
  )
import Tidepool.Wasm.Runner (WasmResult(..), initializeWasm)
import Tidepool.Wasm.Ffi.Types (mkErrorOutput)
import Tidepool.Wasm.TestGraph (computeHandlerWasm)
import Tidepool.Wasm.Registry.Types (GraphEntry(..), ActiveSession(..))


-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH ENTRY
-- ════════════════════════════════════════════════════════════════════════════

testGraphEntry :: GraphEntry
testGraphEntry = GraphEntry
  { geName = "TestGraph"
  , geGraphInfo = graphInfo
  , geCreate = createSession
  }


-- ════════════════════════════════════════════════════════════════════════════
-- STATIC METADATA
-- ════════════════════════════════════════════════════════════════════════════

graphInfo :: Value
graphInfo = object
  [ "name" .= ("TestGraph" :: Text)
  , "id" .= ("test" :: Text)
  , "nodes" .= (["entry", "compute", "exit"] :: [Text])
  , "edges" .= [object ["from" .= ("entry" :: Text), "to" .= ("compute" :: Text)]
               ,object ["from" .= ("compute" :: Text), "to" .= ("exit" :: Text)]]
  ]


-- ════════════════════════════════════════════════════════════════════════════
-- SESSION STATE (per-session, not global)
-- ════════════════════════════════════════════════════════════════════════════

-- | Internal state for a single session.
-- Each session gets its own IORef - no global state.
data TestState
  = TestIdle
  | TestWaiting (EffectResult -> WasmResult (GotoChoice '[To Exit Int])) ExecutionPhase


-- ════════════════════════════════════════════════════════════════════════════
-- SESSION CREATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Convert exit type to StepOutput
exitToOutput :: GotoChoice '[To Exit Int] -> StepOutput
exitToOutput (GotoChoice (Here result)) =
  let resultVal = toJSON result
  in StepDone resultVal (GraphState (PhaseCompleted resultVal) ["compute"])


-- | Create a new session for TestGraph.
--
-- Each call allocates a fresh IORef for this session's state.
-- When the session is replaced in Registry.activeSession, this IORef
-- becomes unreachable and can be garbage collected.
createSession :: Value -> IO (Either Text (StepOutput, ActiveSession))
createSession inputValue = do
  -- Allocate fresh IORef for THIS session (not global!)
  sessionState <- newIORef TestIdle

  case parseIntValue inputValue of
    Left err -> pure $ Left $ "Invalid input for TestGraph: " <> err
    Right entry -> do
      let result = initializeWasm (computeHandlerWasm entry)
      output <- wasmResultToOutput sessionState result
      let session = ActiveSession
            { asGraphId = "test"
            , asStep = stepSession sessionState
            , asGetState = getSessionState sessionState
            , asGraphInfo = graphInfo
            }
      pure $ Right (output, session)


-- | Parse a JSON Value as an Int
parseIntValue :: Value -> Either Text Int
parseIntValue (Number n) = case floatingOrInteger n of
  Right i -> Right i
  Left (_ :: Double) -> Right (round n)
parseIntValue _ = Left "Expected integer"


-- | Step the session (captures sessionState IORef in closure)
stepSession :: IORef TestState -> EffectResult -> IO StepOutput
stepSession sessionState effectResult = do
  st <- readIORef sessionState
  case st of
    TestIdle -> pure $ mkErrorOutput "TestGraph not initialized"
    TestWaiting resume _phase -> do
      let result = resume effectResult
      wasmResultToOutput sessionState result


-- | Get current state (captures sessionState IORef in closure)
getSessionState :: IORef TestState -> IO GraphState
getSessionState sessionState = do
  st <- readIORef sessionState
  pure $ case st of
    TestIdle -> GraphState PhaseIdle []
    TestWaiting _ phase -> GraphState phase []


-- | Convert WasmResult to StepOutput and update session state
wasmResultToOutput :: IORef TestState -> WasmResult (GotoChoice '[To Exit Int]) -> IO StepOutput
wasmResultToOutput sessionState (WasmComplete a) = do
  writeIORef sessionState TestIdle
  pure $ exitToOutput a

wasmResultToOutput sessionState (WasmYield eff resume) = do
  let phase = PhaseInNode "compute"
  writeIORef sessionState (TestWaiting resume phase)
  pure $ StepYield eff (GraphState phase [])

wasmResultToOutput sessionState (WasmError msg) = do
  writeIORef sessionState TestIdle
  pure $ mkErrorOutput msg
