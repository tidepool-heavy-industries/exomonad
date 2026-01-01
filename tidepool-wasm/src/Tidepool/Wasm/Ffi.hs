{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | WASM FFI exports for graph execution.
--
-- These are the entry points that TypeScript calls into the WASM module.
-- The functions are exported when compiling for WASM target (wasm32_HOST_ARCH).
--
-- For native builds, we provide the same interface without FFI exports,
-- allowing the same code to be tested natively.
--
-- = Continuation Storage
--
-- The continuation returned by 'WasmYield' is a Haskell function that cannot
-- be serialized to JSON. We store it in a global 'IORef' (per WASM instance).
-- This works because:
--
-- * Each WASM module instance is isolated (one per Durable Object)
-- * Single-threaded execution (no races)
-- * The continuation lives for the duration of the yield/resume cycle
module Tidepool.Wasm.Ffi
  ( -- * FFI Exports (TestGraph)
    initialize
  , step
  , getGraphInfo
  , getGraphState
    -- * FFI Exports (ExampleGraph)
  , initializeExample
  , stepExample
  , getExampleGraphInfo
  , getExampleGraphState
    -- * Testing (native only)
  , resetState
  , resetExampleState
  ) where

import Data.Aeson (eitherDecodeStrict, encode, object, (.=), toJSON)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import System.IO.Unsafe (unsafePerformIO)

import Tidepool.Wasm.Runner (WasmResult(..), initializeWasm)
import Tidepool.Wasm.TestGraph (computeHandlerWasm)
import Tidepool.Wasm.ExampleGraph (UserMessage(..), Response(..), runExampleGraph)
import Tidepool.Wasm.HabiticaRoutingGraph (RawInput(..), ExecutionResult(..), runHabiticaRoutingGraph)
import Tidepool.Wasm.HabiticaTypes ()
import Tidepool.Wasm.WireTypes
  ( EffectResult(..)
  , ExecutionPhase(..)
  , GraphState(..)
  , StepOutput(..)  -- StepYield, StepDone, StepFailed
  )
import Tidepool.Graph.Goto (GotoChoice(..), OneOf(..), To)
import Tidepool.Graph.Types (Exit)

#if defined(wasm32_HOST_ARCH)
import GHC.Wasm.Prim (JSString(..), fromJSString, toJSString)
#endif


-- ════════════════════════════════════════════════════════════════════════════
-- FFI EXPORTS (WASM target)
-- ════════════════════════════════════════════════════════════════════════════

#if defined(wasm32_HOST_ARCH)

-- | Start graph execution with JSON input.
--
-- Takes JSON-encoded entry value, returns JSON-encoded StepOutput.
-- The StepOutput contains either:
-- - An effect to execute (caller should handle, then call 'step')
-- - done=True with final result
foreign export javascript "initialize" initialize :: JSString -> IO JSString

-- | Continue graph execution with effect result.
--
-- Takes JSON-encoded EffectResult from the previous effect.
-- Returns JSON-encoded StepOutput for next step.
foreign export javascript "step" step :: JSString -> IO JSString

-- | Get compile-time graph structure.
--
-- Returns JSON-encoded GraphInfo describing nodes, edges, types.
foreign export javascript "getGraphInfo" getGraphInfo :: IO JSString

-- | Get current runtime graph state.
--
-- Returns JSON-encoded GraphState showing execution progress.
foreign export javascript "getGraphState" getGraphState :: IO JSString

-- ExampleGraph FFI exports

-- | Start ExampleGraph execution with JSON input (Text message).
foreign export javascript "initializeExample" initializeExample :: JSString -> IO JSString

-- | Continue ExampleGraph execution with effect result.
foreign export javascript "stepExample" stepExample :: JSString -> IO JSString

-- | Get ExampleGraph structure.
foreign export javascript "getExampleGraphInfo" getExampleGraphInfo :: IO JSString

-- | Get ExampleGraph runtime state.
foreign export javascript "getExampleGraphState" getExampleGraphState :: IO JSString

-- HabiticaRoutingGraph FFI exports

-- | Start HabiticaRoutingGraph execution with JSON input (RawInput).
foreign export javascript "initializeHabitica" initializeHabitica :: JSString -> IO JSString

-- | Continue HabiticaRoutingGraph execution with effect result.
foreign export javascript "stepHabitica" stepHabitica :: JSString -> IO JSString

-- | Get HabiticaRoutingGraph structure.
foreign export javascript "getHabiticaGraphInfo" getHabiticaGraphInfo :: IO JSString

-- | Get HabiticaRoutingGraph runtime state.
foreign export javascript "getHabiticaGraphState" getHabiticaGraphState :: IO JSString

#endif


-- ════════════════════════════════════════════════════════════════════════════
-- GLOBAL STATE
-- ════════════════════════════════════════════════════════════════════════════

-- | Existential wrapper for the continuation.
--
-- The continuation's result type varies by graph, so we wrap it existentially.
-- The 'toOutput' function converts the final value to StepOutput.
data SomeContinuation where
  SomeCont
    :: (EffectResult -> WasmResult a)  -- The continuation
    -> (a -> StepOutput)               -- How to convert result to StepOutput
    -> SomeContinuation

-- | Global state: either idle or waiting for an effect result.
data WasmState
  = Idle
  | Waiting SomeContinuation ExecutionPhase

-- | Global state persisted across FFI calls.
--
-- In WASM: Each module instance is isolated (one per Durable Object),
-- so "global" really means "session-scoped". Single-threaded, no races.
--
-- Note: NOINLINE is critical - ensures single IORef across all call sites.
{-# NOINLINE globalState #-}
globalState :: IORef WasmState
globalState = unsafePerformIO $ newIORef Idle


-- | Reset state (for testing only).
resetState :: IO ()
resetState = writeIORef globalState Idle


-- | Global state for ExampleGraph (separate from TestGraph).
{-# NOINLINE exampleGlobalState #-}
exampleGlobalState :: IORef WasmState
exampleGlobalState = unsafePerformIO $ newIORef Idle


-- | Reset ExampleGraph state (for testing only).
resetExampleState :: IO ()
resetExampleState = writeIORef exampleGlobalState Idle


-- | Global state for HabiticaRoutingGraph.
{-# NOINLINE habiticaGlobalState #-}
habiticaGlobalState :: IORef WasmState
habiticaGlobalState = unsafePerformIO $ newIORef Idle


-- | Reset HabiticaRoutingGraph state (for testing only).
resetHabiticaState :: IO ()
resetHabiticaState = writeIORef habiticaGlobalState Idle


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Encode StepOutput to JSON Text.
encodeStepOutput :: StepOutput -> Text
encodeStepOutput = TL.toStrict . TLE.decodeUtf8 . encode

-- | Create error StepOutput.
mkErrorOutput :: Text -> StepOutput
mkErrorOutput msg = StepFailed msg (GraphState (PhaseFailed msg) [])

-- | Node name for TestGraph's compute node.
testGraphNodeName :: Text
testGraphNodeName = "compute"

-- | Convert a GotoChoice result to StepOutput.
--
-- Note: This is specialized to TestGraph's result type. For other graphs,
-- you would need additional converters or a more generic approach.
gotoChoiceToOutput :: GotoChoice '[To Exit Int] -> StepOutput
gotoChoiceToOutput (GotoChoice (Here result)) =
  let resultVal = toJSON result
  in StepDone resultVal (GraphState (PhaseCompleted resultVal) [testGraphNodeName])

-- | Convert WasmResult to StepOutput, storing continuation if yielded.
--
-- Note: Specialized to TestGraph's result type.
wasmResultToOutput
  :: WasmResult (GotoChoice '[To Exit Int])
  -> IO StepOutput
wasmResultToOutput (WasmComplete choice) = do
  writeIORef globalState Idle
  pure $ gotoChoiceToOutput choice

wasmResultToOutput (WasmYield eff resume) = do
  let phase = PhaseInNode testGraphNodeName
  writeIORef globalState (Waiting (SomeCont resume gotoChoiceToOutput) phase)
  pure $ StepYield eff (GraphState phase [])

wasmResultToOutput (WasmError msg) = do
  writeIORef globalState Idle
  pure $ mkErrorOutput msg


-- ════════════════════════════════════════════════════════════════════════════
-- IMPLEMENTATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Initialize graph execution.
--
-- 1. Parses entry value (Int) from JSON
-- 2. Runs graph until first effect yield (using freer-simple)
-- 3. Stores continuation in global state
-- 4. Returns StepOutput with effect to execute
#if defined(wasm32_HOST_ARCH)
initialize :: JSString -> IO JSString
initialize input = toJSString . T.unpack <$> initializeImpl (T.pack $ fromJSString input)
#else
initialize :: Text -> IO Text
initialize = initializeImpl
#endif

initializeImpl :: Text -> IO Text
initializeImpl inputJson =
  case eitherDecodeStrict (encodeUtf8 inputJson) of
    Left err -> pure $ encodeStepOutput $ mkErrorOutput $ "JSON parse error: " <> T.pack err
    Right entry -> do
      let result = initializeWasm (computeHandlerWasm entry)
      output <- wasmResultToOutput result
      pure $ encodeStepOutput output


-- | Step graph execution with effect result.
--
-- 1. Loads continuation from global state
-- 2. Parses EffectResult from JSON
-- 3. Resumes graph execution with result
-- 4. Stores new continuation or clears state on completion
-- 5. Returns StepOutput
#if defined(wasm32_HOST_ARCH)
step :: JSString -> IO JSString
step result = toJSString . T.unpack <$> stepImpl (T.pack $ fromJSString result)
#else
step :: Text -> IO Text
step = stepImpl
#endif

stepImpl :: Text -> IO Text
stepImpl resultJson = do
  mState <- readIORef globalState
  case mState of
    Idle ->
      pure $ encodeStepOutput $ mkErrorOutput "Graph not initialized - call initialize() before step()"
    Waiting (SomeCont resume toOutput) _phase ->
      case eitherDecodeStrict (encodeUtf8 resultJson) of
        Left err ->
          pure $ encodeStepOutput $ mkErrorOutput $ "JSON parse error: " <> T.pack err
        Right effectResult -> do
          let nextResult = resume effectResult
          -- We need to handle the result generically, but for now we know it's TestGraph
          output <- wasmResultToOutputGeneric nextResult toOutput
          pure $ encodeStepOutput output

-- | Generic version that uses the stored toOutput function.
wasmResultToOutputGeneric
  :: WasmResult a
  -> (a -> StepOutput)
  -> IO StepOutput
wasmResultToOutputGeneric (WasmComplete a) toOutput = do
  writeIORef globalState Idle
  pure $ toOutput a

wasmResultToOutputGeneric (WasmYield eff resume) toOutput = do
  let phase = PhaseInNode testGraphNodeName
  writeIORef globalState (Waiting (SomeCont resume toOutput) phase)
  pure $ StepYield eff (GraphState phase [])

wasmResultToOutputGeneric (WasmError msg) _toOutput = do
  writeIORef globalState Idle
  pure $ mkErrorOutput msg


-- | Get graph metadata.
--
-- Returns static graph structure. For TestGraph:
-- Entry(Int) → compute → Exit(Int)
#if defined(wasm32_HOST_ARCH)
getGraphInfo :: IO JSString
getGraphInfo = toJSString . T.unpack <$> getGraphInfoImpl
#else
getGraphInfo :: IO Text
getGraphInfo = getGraphInfoImpl
#endif

getGraphInfoImpl :: IO Text
getGraphInfoImpl =
  -- Static graph info for TestGraph
  pure $ TL.toStrict $ TLE.decodeUtf8 $ encode $
    object
      [ "name" .= ("TestGraph" :: Text)
      , "nodes" .= (["entry", "compute", "exit"] :: [Text])
      , "edges" .= object
          [ "entry" .= ("compute" :: Text)
          , "compute" .= ("exit" :: Text)
          ]
      ]


-- | Get current graph state.
--
-- Returns runtime state from global IORef.
#if defined(wasm32_HOST_ARCH)
getGraphState :: IO JSString
getGraphState = toJSString . T.unpack <$> getGraphStateImpl
#else
getGraphState :: IO Text
getGraphState = getGraphStateImpl
#endif

getGraphStateImpl :: IO Text
getGraphStateImpl = do
  mState <- readIORef globalState
  let graphState = case mState of
        Idle -> GraphState PhaseIdle []
        Waiting _ phase -> GraphState phase []
  pure $ TL.toStrict $ TLE.decodeUtf8 $ encode graphState


-- ════════════════════════════════════════════════════════════════════════════
-- EXAMPLE GRAPH IMPLEMENTATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Node name for tracking ExampleGraph execution.
exampleGraphNodeName :: Text
exampleGraphNodeName = "classify"

-- | Convert Response result to StepOutput.
responseToOutput :: Response -> StepOutput
responseToOutput resp =
  let resultVal = toJSON resp.unResponse
  in StepDone resultVal (GraphState (PhaseCompleted resultVal) [exampleGraphNodeName])

-- | Convert WasmResult for ExampleGraph to StepOutput.
exampleWasmResultToOutput :: WasmResult Response -> IO StepOutput
exampleWasmResultToOutput (WasmComplete resp) = do
  writeIORef exampleGlobalState Idle
  pure $ responseToOutput resp

exampleWasmResultToOutput (WasmYield eff resume) = do
  let phase = PhaseInNode exampleGraphNodeName
  writeIORef exampleGlobalState (Waiting (SomeCont resume responseToOutput) phase)
  pure $ StepYield eff (GraphState phase [])

exampleWasmResultToOutput (WasmError msg) = do
  writeIORef exampleGlobalState Idle
  pure $ mkErrorOutput msg


-- | Initialize ExampleGraph with a user message.
#if defined(wasm32_HOST_ARCH)
initializeExample :: JSString -> IO JSString
initializeExample input = toJSString . T.unpack <$> initializeExampleImpl (T.pack $ fromJSString input)
#else
initializeExample :: Text -> IO Text
initializeExample = initializeExampleImpl
#endif

initializeExampleImpl :: Text -> IO Text
initializeExampleImpl inputJson =
  case eitherDecodeStrict (encodeUtf8 inputJson) of
    Left err -> pure $ encodeStepOutput $ mkErrorOutput $ "JSON parse error: " <> T.pack err
    Right msgText -> do
      let result = initializeWasm (runExampleGraph (UserMessage msgText))
      output <- exampleWasmResultToOutput result
      pure $ encodeStepOutput output


-- | Step ExampleGraph execution.
#if defined(wasm32_HOST_ARCH)
stepExample :: JSString -> IO JSString
stepExample result = toJSString . T.unpack <$> stepExampleImpl (T.pack $ fromJSString result)
#else
stepExample :: Text -> IO Text
stepExample = stepExampleImpl
#endif

stepExampleImpl :: Text -> IO Text
stepExampleImpl resultJson = do
  mState <- readIORef exampleGlobalState
  case mState of
    Idle ->
      pure $ encodeStepOutput $ mkErrorOutput "ExampleGraph not initialized - call initializeExample() before stepExample()"
    Waiting (SomeCont resume toOutput) _phase ->
      case eitherDecodeStrict (encodeUtf8 resultJson) of
        Left err ->
          pure $ encodeStepOutput $ mkErrorOutput $ "JSON parse error: " <> T.pack err
        Right effectResult -> do
          let nextResult = resume effectResult
          output <- wasmResultToOutputGenericExample nextResult toOutput
          pure $ encodeStepOutput output

-- | Generic version for ExampleGraph.
wasmResultToOutputGenericExample
  :: WasmResult a
  -> (a -> StepOutput)
  -> IO StepOutput
wasmResultToOutputGenericExample (WasmComplete a) toOutput = do
  writeIORef exampleGlobalState Idle
  pure $ toOutput a

wasmResultToOutputGenericExample (WasmYield eff resume) toOutput = do
  let phase = PhaseInNode exampleGraphNodeName
  writeIORef exampleGlobalState (Waiting (SomeCont resume toOutput) phase)
  pure $ StepYield eff (GraphState phase [])

wasmResultToOutputGenericExample (WasmError msg) _toOutput = do
  writeIORef exampleGlobalState Idle
  pure $ mkErrorOutput msg


-- | Get ExampleGraph metadata.
#if defined(wasm32_HOST_ARCH)
getExampleGraphInfo :: IO JSString
getExampleGraphInfo = toJSString . T.unpack <$> getExampleGraphInfoImpl
#else
getExampleGraphInfo :: IO Text
getExampleGraphInfo = getExampleGraphInfoImpl
#endif

getExampleGraphInfoImpl :: IO Text
getExampleGraphInfoImpl =
  pure $ TL.toStrict $ TLE.decodeUtf8 $ encode $
    object
      [ "name" .= ("ExampleGraph" :: Text)
      , "description" .= ("Message classifier with branching" :: Text)
      , "nodes" .= (["entry", "classify", "handleGreeting", "handleQuestion", "handleStatement", "exit"] :: [Text])
      , "edges" .= object
          [ "entry" .= ("classify" :: Text)
          , "classify" .= (["handleGreeting", "handleQuestion", "handleStatement"] :: [Text])
          , "handleGreeting" .= ("exit" :: Text)
          , "handleQuestion" .= (["exit", "handleQuestion"] :: [Text])
          , "handleStatement" .= ("exit" :: Text)
          ]
      ]


-- | Get ExampleGraph runtime state.
#if defined(wasm32_HOST_ARCH)
getExampleGraphState :: IO JSString
getExampleGraphState = toJSString . T.unpack <$> getExampleGraphStateImpl
#else
getExampleGraphState :: IO Text
getExampleGraphState = getExampleGraphStateImpl
#endif

getExampleGraphStateImpl :: IO Text
getExampleGraphStateImpl = do
  mState <- readIORef exampleGlobalState
  let graphState = case mState of
        Idle -> GraphState PhaseIdle []
        Waiting _ phase -> GraphState phase []
  pure $ TL.toStrict $ TLE.decodeUtf8 $ encode graphState


-- ════════════════════════════════════════════════════════════════════════════
-- HABITICA ROUTING GRAPH IMPLEMENTATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Node name for HabiticaRoutingGraph (for phase tracking).
habiticaGraphNodeName :: Text
habiticaGraphNodeName = "habiticaRouting"

-- | Convert HabiticaRoutingGraph result to StepOutput.
habiticaResultToOutput :: ExecutionResult -> StepOutput
habiticaResultToOutput result =
  StepDone (toJSON result) (GraphState (PhaseCompleted (toJSON result)) [])

-- | Initialize HabiticaRoutingGraph with RawInput.
#if defined(wasm32_HOST_ARCH)
initializeHabitica :: JSString -> IO JSString
initializeHabitica input = toJSString . T.unpack <$> initializeHabiticaImpl (T.pack $ fromJSString input)
#else
initializeHabitica :: Text -> IO Text
initializeHabitica = initializeHabiticaImpl
#endif

initializeHabiticaImpl :: Text -> IO Text
initializeHabiticaImpl inputJson =
  case eitherDecodeStrict (encodeUtf8 inputJson) of
    Left err -> pure $ encodeStepOutput $ mkErrorOutput $ "JSON parse error: " <> T.pack err
    Right rawInput -> do
      let result = initializeWasm (runHabiticaRoutingGraph rawInput)
      output <- habiticaWasmResultToOutput result
      pure $ encodeStepOutput output

-- | Step HabiticaRoutingGraph execution.
#if defined(wasm32_HOST_ARCH)
stepHabitica :: JSString -> IO JSString
stepHabitica result = toJSString . T.unpack <$> stepHabiticaImpl (T.pack $ fromJSString result)
#else
stepHabitica :: Text -> IO Text
stepHabitica = stepHabiticaImpl
#endif

stepHabiticaImpl :: Text -> IO Text
stepHabiticaImpl resultJson = do
  mState <- readIORef habiticaGlobalState
  case mState of
    Idle ->
      pure $ encodeStepOutput $ mkErrorOutput "HabiticaRoutingGraph not initialized - call initializeHabitica() before stepHabitica()"
    Waiting (SomeCont resume toOutput) _phase ->
      case eitherDecodeStrict (encodeUtf8 resultJson) of
        Left err ->
          pure $ encodeStepOutput $ mkErrorOutput $ "JSON parse error: " <> T.pack err
        Right effectResult -> do
          let nextResult = resume effectResult
          output <- wasmResultToOutputGenericHabitica nextResult toOutput
          pure $ encodeStepOutput output

-- | Generic version for HabiticaRoutingGraph.
wasmResultToOutputGenericHabitica
  :: WasmResult a
  -> (a -> StepOutput)
  -> IO StepOutput
wasmResultToOutputGenericHabitica (WasmComplete a) toOutput = do
  writeIORef habiticaGlobalState Idle
  pure $ toOutput a

wasmResultToOutputGenericHabitica (WasmYield eff resume) toOutput = do
  let phase = PhaseInNode habiticaGraphNodeName
  writeIORef habiticaGlobalState (Waiting (SomeCont resume toOutput) phase)
  pure $ StepYield eff (GraphState phase [])

wasmResultToOutputGenericHabitica (WasmError msg) _toOutput = do
  writeIORef habiticaGlobalState Idle
  pure $ mkErrorOutput msg

-- | Convert WasmResult to StepOutput for HabiticaRoutingGraph.
habiticaWasmResultToOutput :: WasmResult ExecutionResult -> IO StepOutput
habiticaWasmResultToOutput (WasmComplete result) = do
  writeIORef habiticaGlobalState Idle
  pure $ habiticaResultToOutput result

habiticaWasmResultToOutput (WasmYield eff resume) = do
  let phase = PhaseInNode habiticaGraphNodeName
  writeIORef habiticaGlobalState (Waiting (SomeCont resume habiticaResultToOutput) phase)
  pure $ StepYield eff (GraphState phase [])

habiticaWasmResultToOutput (WasmError msg) = do
  writeIORef habiticaGlobalState Idle
  pure $ mkErrorOutput msg


-- | Get HabiticaRoutingGraph metadata.
#if defined(wasm32_HOST_ARCH)
getHabiticaGraphInfo :: IO JSString
getHabiticaGraphInfo = toJSString . T.unpack <$> getHabiticaGraphInfoImpl
#else
getHabiticaGraphInfo :: IO Text
getHabiticaGraphInfo = getHabiticaGraphInfoImpl
#endif

getHabiticaGraphInfoImpl :: IO Text
getHabiticaGraphInfoImpl =
  pure $ TL.toStrict $ TLE.decodeUtf8 $ encode $
    object
      [ "name" .= ("HabiticaRoutingGraph" :: Text)
      , "description" .= ("Routes extracted tasks to Habitica with user confirmation" :: Text)
      , "nodes" .= (["entry", "extractTask", "fetchExisting", "matchTask", "suggestAction", "confirmWithUser", "executeAction", "exit"] :: [Text])
      ]


-- | Get HabiticaRoutingGraph runtime state.
#if defined(wasm32_HOST_ARCH)
getHabiticaGraphState :: IO JSString
getHabiticaGraphState = toJSString . T.unpack <$> getHabiticaGraphStateImpl
#else
getHabiticaGraphState :: IO Text
getHabiticaGraphState = getHabiticaGraphStateImpl
#endif

getHabiticaGraphStateImpl :: IO Text
getHabiticaGraphStateImpl = do
  mState <- readIORef habiticaGlobalState
  let graphState = case mState of
        Idle -> GraphState PhaseIdle []
        Waiting _ phase -> GraphState phase []
  pure $ TL.toStrict $ TLE.decodeUtf8 $ encode graphState
