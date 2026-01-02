{-# LANGUAGE OverloadedStrings #-}

-- | ExampleGraph registry entry.
--
-- Uses per-session IORef pattern: each createSession allocates a fresh IORef
-- that lives only as long as the session. This avoids state leakage between
-- graphs when switching sessions.
module Tidepool.Wasm.Registry.ExampleGraph
  ( exampleGraphEntry
  ) where

import Data.Aeson (ToJSON(..), Value(..), object, (.=), eitherDecodeStrict)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Tidepool.Wasm.WireTypes
  ( EffectResult(..)
  , ExecutionPhase(..)
  , GraphState(..)
  , StepOutput(..)
  )
import Tidepool.Wasm.Runner (WasmResult(..), initializeWasm)
import Tidepool.Wasm.Ffi.Types (mkErrorOutput)
import Tidepool.Wasm.ExampleGraph (Response(..), runExampleGraph, UserMessage(..))
import Tidepool.Wasm.Registry.Types (GraphEntry(..), ActiveSession(..))


-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH ENTRY
-- ════════════════════════════════════════════════════════════════════════════

exampleGraphEntry :: GraphEntry
exampleGraphEntry = GraphEntry
  { geName = "ExampleGraph"
  , geGraphInfo = graphInfo
  , geCreate = createSession
  }


-- ════════════════════════════════════════════════════════════════════════════
-- STATIC METADATA
-- ════════════════════════════════════════════════════════════════════════════

graphInfo :: Value
graphInfo = object
  [ "name" .= ("ExampleGraph" :: Text)
  , "id" .= ("example" :: Text)
  , "nodes" .= (["entry", "classify", "handleGreeting", "handleQuestion", "handleStatement", "exit"] :: [Text])
  , "edges" .=
      [ object ["from" .= ("entry" :: Text), "to" .= ("classify" :: Text)]
      , object ["from" .= ("classify" :: Text), "to" .= ("handleGreeting" :: Text)]
      , object ["from" .= ("classify" :: Text), "to" .= ("handleQuestion" :: Text)]
      , object ["from" .= ("classify" :: Text), "to" .= ("handleStatement" :: Text)]
      , object ["from" .= ("handleGreeting" :: Text), "to" .= ("exit" :: Text)]
      , object ["from" .= ("handleQuestion" :: Text), "to" .= ("exit" :: Text)]
      , object ["from" .= ("handleStatement" :: Text), "to" .= ("exit" :: Text)]
      ]
  ]


-- ════════════════════════════════════════════════════════════════════════════
-- SESSION STATE (per-session, not global)
-- ════════════════════════════════════════════════════════════════════════════

-- | Internal state for a single session.
-- Each session gets its own IORef - no global state.
data ExampleState
  = ExampleIdle
  | ExampleWaiting (EffectResult -> WasmResult Response) ExecutionPhase


-- ════════════════════════════════════════════════════════════════════════════
-- SESSION CREATION
-- ════════════════════════════════════════════════════════════════════════════

exitToOutput :: Response -> StepOutput
exitToOutput resp =
  let resultVal = toJSON resp.unResponse
  in StepDone resultVal (GraphState (PhaseCompleted resultVal) ["classify"])


-- | Create a new session for ExampleGraph.
--
-- Each call allocates a fresh IORef for this session's state.
-- When the session is replaced in Registry.activeSession, this IORef
-- becomes unreachable and can be garbage collected.
createSession :: Value -> IO (Either Text (StepOutput, ActiveSession))
createSession inputValue = do
  -- Allocate fresh IORef for THIS session (not global!)
  sessionState <- newIORef ExampleIdle

  case parseUserMessage inputValue of
    Left err -> pure $ Left $ "Invalid input for ExampleGraph: " <> err
    Right msg -> do
      let result = initializeWasm (runExampleGraph msg)
      output <- wasmResultToOutput sessionState result
      let session = ActiveSession
            { asGraphId = "example"
            , asStep = stepSession sessionState
            , asGetState = getSessionState sessionState
            , asGraphInfo = graphInfo
            }
      pure $ Right (output, session)


parseUserMessage :: Value -> Either Text UserMessage
parseUserMessage (String t) = Right (UserMessage t)
parseUserMessage (Object o) =
  -- Try to extract messageText field for compatibility
  case eitherDecodeStrict (encodeUtf8 $ T.pack $ show o) of
    Left _ -> Left "Expected string or object with messageText"
    Right (msg :: UserMessage) -> Right msg
parseUserMessage _ = Left "Expected string or object"


-- | Step the session (captures sessionState IORef in closure)
stepSession :: IORef ExampleState -> EffectResult -> IO StepOutput
stepSession sessionState effectResult = do
  st <- readIORef sessionState
  case st of
    ExampleIdle -> pure $ mkErrorOutput "ExampleGraph not initialized"
    ExampleWaiting resume _phase -> do
      let result = resume effectResult
      wasmResultToOutput sessionState result


-- | Get current state (captures sessionState IORef in closure)
getSessionState :: IORef ExampleState -> IO GraphState
getSessionState sessionState = do
  st <- readIORef sessionState
  pure $ case st of
    ExampleIdle -> GraphState PhaseIdle []
    ExampleWaiting _ phase -> GraphState phase []


-- | Convert WasmResult to StepOutput and update session state
wasmResultToOutput :: IORef ExampleState -> WasmResult Response -> IO StepOutput
wasmResultToOutput sessionState (WasmComplete a) = do
  writeIORef sessionState ExampleIdle
  pure $ exitToOutput a

wasmResultToOutput sessionState (WasmYield eff resume) = do
  let phase = PhaseInNode "classify"
  writeIORef sessionState (ExampleWaiting resume phase)
  pure $ StepYield eff (GraphState phase [])

wasmResultToOutput sessionState (WasmError msg) = do
  writeIORef sessionState ExampleIdle
  pure $ mkErrorOutput msg
