{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-- | WASM FFI exports for graph execution.
--
-- This module uses Template Haskell to generate FFI exports for all graphs.
-- The pattern:
--
-- 1. Each graph has an exit-to-output converter and a state IORef
-- 2. The TH splice generates initialize/step/getGraphInfo/getGraphState
-- 3. Foreign export declarations are added via CPP
--
-- = Adding a New Graph
--
-- 1. Create the graph module (e.g., NewGraph.hs) with a runner function
-- 2. Add to this module:
--    - State IORef: @newGraphState :: IORef WasmState@
--    - Exit converter: @newExitToOutput :: ResultType -> StepOutput@
--    - GraphFFISpec entry in the TH splice
-- 3. Add foreign export declarations in the CPP section
-- 4. Add WASM export flags in tidepool-wasm.cabal
module Tidepool.Wasm.Ffi
  ( -- * FFI Exports (TestGraph)
    initialize_test
  , step_test
  , getGraphInfo_test
  , getGraphState_test

    -- * FFI Exports (ExampleGraph)
  , initialize_example
  , step_example
  , getGraphInfo_example
  , getGraphState_example

    -- * FFI Exports (HabiticaRoutingGraph)
  , initialize_habitica
  , step_habitica
  , getGraphInfo_habitica
  , getGraphState_habitica

    -- * Testing (native only)
  , resetTestState
  , resetExampleState
  , resetHabiticaState
  ) where

import Data.Aeson (toJSON, encode, eitherDecodeStrict, object, (.=))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import System.IO.Unsafe (unsafePerformIO)

import Tidepool.Wasm.Ffi.Types
import Tidepool.Wasm.TestGraph (computeHandlerWasm)
import Tidepool.Wasm.ExampleGraph (Response(..), runExampleGraph)
import Tidepool.Wasm.HabiticaRoutingGraph (ExecutionResult(..), runHabiticaRoutingGraph)
import Tidepool.Wasm.HabiticaTypes ()  -- For orphan instances
import Tidepool.Graph.Goto (GotoChoice(..), OneOf(..), To)
import Tidepool.Graph.Types (Exit)

import Tidepool.Generated.Registry.TH (GraphFFISpec(..), deriveFFIExports)

#if defined(wasm32_HOST_ARCH)
import GHC.Wasm.Prim (JSString(..), fromJSString, toJSString)
#endif


-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH STATE REFS (one per graph)
-- ════════════════════════════════════════════════════════════════════════════

-- | Global state for TestGraph.
{-# NOINLINE testGraphState #-}
testGraphState :: IORef WasmState
testGraphState = unsafePerformIO $ newIORef Idle

-- | Global state for ExampleGraph.
{-# NOINLINE exampleGraphState #-}
exampleGraphState :: IORef WasmState
exampleGraphState = unsafePerformIO $ newIORef Idle

-- | Global state for HabiticaRoutingGraph.
{-# NOINLINE habiticaGraphState #-}
habiticaGraphState :: IORef WasmState
habiticaGraphState = unsafePerformIO $ newIORef Idle


-- ════════════════════════════════════════════════════════════════════════════
-- EXIT-TO-OUTPUT CONVERTERS (one per graph)
-- ════════════════════════════════════════════════════════════════════════════

-- | Convert TestGraph's exit type to StepOutput.
testExitToOutput :: GotoChoice '[To Exit Int] -> StepOutput
testExitToOutput (GotoChoice (Here result)) =
  let resultVal = toJSON result
  in StepDone resultVal (GraphState (PhaseCompleted resultVal) ["compute"])

-- | Convert ExampleGraph's exit type to StepOutput.
exampleExitToOutput :: Response -> StepOutput
exampleExitToOutput resp =
  let resultVal = toJSON resp.unResponse
  in StepDone resultVal (GraphState (PhaseCompleted resultVal) ["classify"])

-- | Convert HabiticaRoutingGraph's exit type to StepOutput.
habiticaExitToOutput :: ExecutionResult -> StepOutput
habiticaExitToOutput result =
  let resultVal = toJSON result
  in StepDone resultVal (GraphState (PhaseCompleted resultVal) ["executeAction"])


-- ════════════════════════════════════════════════════════════════════════════
-- TESTING HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Reset TestGraph state (for testing only).
resetTestState :: IO ()
resetTestState = writeIORef testGraphState Idle

-- | Reset ExampleGraph state (for testing only).
resetExampleState :: IO ()
resetExampleState = writeIORef exampleGraphState Idle

-- | Reset HabiticaRoutingGraph state (for testing only).
resetHabiticaState :: IO ()
resetHabiticaState = writeIORef habiticaGraphState Idle


-- ════════════════════════════════════════════════════════════════════════════
-- TH-GENERATED FFI EXPORTS
-- ════════════════════════════════════════════════════════════════════════════

$(deriveFFIExports
    [ GraphFFISpec
        "test"
        "TestGraph"
        'computeHandlerWasm
        'testGraphState
        'testExitToOutput
        "compute"
        ["entry", "compute", "exit"]
        [("entry", "compute"), ("compute", "exit")]
    , GraphFFISpec
        "example"
        "ExampleGraph"
        'runExampleGraph
        'exampleGraphState
        'exampleExitToOutput
        "classify"
        ["entry", "classify", "handleGreeting", "handleQuestion", "handleStatement", "exit"]
        [ ("entry", "classify")
        , ("classify", "handleGreeting")
        , ("classify", "handleQuestion")
        , ("classify", "handleStatement")
        , ("handleGreeting", "exit")
        , ("handleQuestion", "exit")
        , ("handleStatement", "exit")
        ]
    , GraphFFISpec
        "habitica"
        "HabiticaRoutingGraph"
        'runHabiticaRoutingGraph
        'habiticaGraphState
        'habiticaExitToOutput
        "extractTask"
        ["entry", "extractTask", "fetchExisting", "matchTask", "suggestAction", "confirmWithUser", "executeAction", "exit"]
        [ ("entry", "extractTask")
        , ("extractTask", "fetchExisting")
        , ("fetchExisting", "matchTask")
        , ("matchTask", "suggestAction")
        , ("suggestAction", "confirmWithUser")
        , ("confirmWithUser", "executeAction")
        , ("confirmWithUser", "suggestAction")
        , ("confirmWithUser", "exit")
        , ("executeAction", "exit")
        ]
    ])


-- ════════════════════════════════════════════════════════════════════════════
-- FOREIGN EXPORT DECLARATIONS (WASM only)
-- ════════════════════════════════════════════════════════════════════════════

#if defined(wasm32_HOST_ARCH)

-- Note: TH cannot generate foreign export declarations, so we do it manually.
-- The TH generates the implementation functions; we just need to export them.

-- WASM wrappers that convert JSString <-> Text
initialize_test_wasm :: JSString -> IO JSString
initialize_test_wasm input = toJSString <$> initialize_test (fromJSString input)

step_test_wasm :: JSString -> IO JSString
step_test_wasm result = toJSString <$> step_test (fromJSString result)

getGraphInfo_test_wasm :: IO JSString
getGraphInfo_test_wasm = toJSString <$> getGraphInfo_test

getGraphState_test_wasm :: IO JSString
getGraphState_test_wasm = toJSString <$> getGraphState_test

foreign export javascript "initialize_test" initialize_test_wasm :: JSString -> IO JSString
foreign export javascript "step_test" step_test_wasm :: JSString -> IO JSString
foreign export javascript "getGraphInfo_test" getGraphInfo_test_wasm :: IO JSString
foreign export javascript "getGraphState_test" getGraphState_test_wasm :: IO JSString


initialize_example_wasm :: JSString -> IO JSString
initialize_example_wasm input = toJSString <$> initialize_example (fromJSString input)

step_example_wasm :: JSString -> IO JSString
step_example_wasm result = toJSString <$> step_example (fromJSString result)

getGraphInfo_example_wasm :: IO JSString
getGraphInfo_example_wasm = toJSString <$> getGraphInfo_example

getGraphState_example_wasm :: IO JSString
getGraphState_example_wasm = toJSString <$> getGraphState_example

foreign export javascript "initialize_example" initialize_example_wasm :: JSString -> IO JSString
foreign export javascript "step_example" step_example_wasm :: JSString -> IO JSString
foreign export javascript "getGraphInfo_example" getGraphInfo_example_wasm :: IO JSString
foreign export javascript "getGraphState_example" getGraphState_example_wasm :: IO JSString


initialize_habitica_wasm :: JSString -> IO JSString
initialize_habitica_wasm input = toJSString <$> initialize_habitica (fromJSString input)

step_habitica_wasm :: JSString -> IO JSString
step_habitica_wasm result = toJSString <$> step_habitica (fromJSString result)

getGraphInfo_habitica_wasm :: IO JSString
getGraphInfo_habitica_wasm = toJSString <$> getGraphInfo_habitica

getGraphState_habitica_wasm :: IO JSString
getGraphState_habitica_wasm = toJSString <$> getGraphState_habitica

foreign export javascript "initialize_habitica" initialize_habitica_wasm :: JSString -> IO JSString
foreign export javascript "step_habitica" step_habitica_wasm :: JSString -> IO JSString
foreign export javascript "getGraphInfo_habitica" getGraphInfo_habitica_wasm :: IO JSString
foreign export javascript "getGraphState_habitica" getGraphState_habitica_wasm :: IO JSString

#endif
