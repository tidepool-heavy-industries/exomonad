{-# LANGUAGE OverloadedStrings #-}

-- | TestGraph registry entry.
module Tidepool.Wasm.Registry.TestGraph
  ( testGraphEntry
  ) where

import Data.Aeson (ToJSON(..), Value(..), object, (.=))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)

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

-- Import the shared types from Registry (will create this)
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
-- SESSION STATE
-- ════════════════════════════════════════════════════════════════════════════

-- Internal state for this graph's sessions
data TestState
  = TestIdle
  | TestWaiting (EffectResult -> WasmResult (GotoChoice '[To Exit Int])) ExecutionPhase

{-# NOINLINE testState #-}
testState :: IORef TestState
testState = unsafePerformIO $ newIORef TestIdle


-- ════════════════════════════════════════════════════════════════════════════
-- SESSION CREATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Convert exit type to StepOutput
exitToOutput :: GotoChoice '[To Exit Int] -> StepOutput
exitToOutput (GotoChoice (Here result)) =
  let resultVal = toJSON result
  in StepDone resultVal (GraphState (PhaseCompleted resultVal) ["compute"])


-- | Create a new session for TestGraph
createSession :: Value -> IO (Either Text (StepOutput, ActiveSession))
createSession inputValue = do
  -- Reset state
  writeIORef testState TestIdle

  -- Parse input as Int
  case parseIntValue inputValue of
    Left err -> pure $ Left $ "Invalid input for TestGraph: " <> err
    Right entry -> do
      -- Initialize the graph
      let result = initializeWasm (computeHandlerWasm entry)
      output <- wasmResultToOutput result
      let session = ActiveSession
            { asGraphId = "test"
            , asStep = stepSession
            , asGetState = getSessionState
            , asGraphInfo = graphInfo
            }
      pure $ Right (output, session)


-- | Parse a JSON Value as an Int
parseIntValue :: Value -> Either Text Int
parseIntValue (Number n) = case floatingOrInteger n of
  Right i -> Right i
  Left (_ :: Double) -> Right (round n)
parseIntValue _ = Left "Expected integer"


-- | Step the TestGraph session
stepSession :: EffectResult -> IO StepOutput
stepSession effectResult = do
  st <- readIORef testState
  case st of
    TestIdle -> pure $ mkErrorOutput "TestGraph not initialized"
    TestWaiting resume _phase -> do
      let result = resume effectResult
      wasmResultToOutput result


-- | Get current state
getSessionState :: IO GraphState
getSessionState = do
  st <- readIORef testState
  pure $ case st of
    TestIdle -> GraphState PhaseIdle []
    TestWaiting _ phase -> GraphState phase []


-- | Convert WasmResult to StepOutput and update state
wasmResultToOutput :: WasmResult (GotoChoice '[To Exit Int]) -> IO StepOutput
wasmResultToOutput (WasmComplete a) = do
  writeIORef testState TestIdle
  pure $ exitToOutput a

wasmResultToOutput (WasmYield eff resume) = do
  let phase = PhaseInNode "compute"
  writeIORef testState (TestWaiting resume phase)
  pure $ StepYield eff (GraphState phase [])

wasmResultToOutput (WasmError msg) = do
  writeIORef testState TestIdle
  pure $ mkErrorOutput msg
