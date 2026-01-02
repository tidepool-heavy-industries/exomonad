{-# LANGUAGE OverloadedStrings #-}

-- | ExampleGraph registry entry.
module Tidepool.Wasm.Registry.ExampleGraph
  ( exampleGraphEntry
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), object, (.=), eitherDecodeStrict)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import System.IO.Unsafe (unsafePerformIO)

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
-- SESSION STATE
-- ════════════════════════════════════════════════════════════════════════════

data ExampleState
  = ExampleIdle
  | ExampleWaiting (EffectResult -> WasmResult Response) ExecutionPhase

{-# NOINLINE exampleState #-}
exampleState :: IORef ExampleState
exampleState = unsafePerformIO $ newIORef ExampleIdle


-- ════════════════════════════════════════════════════════════════════════════
-- SESSION CREATION
-- ════════════════════════════════════════════════════════════════════════════

exitToOutput :: Response -> StepOutput
exitToOutput resp =
  let resultVal = toJSON resp.unResponse
  in StepDone resultVal (GraphState (PhaseCompleted resultVal) ["classify"])


createSession :: Value -> IO (Either Text (StepOutput, ActiveSession))
createSession inputValue = do
  writeIORef exampleState ExampleIdle

  -- Parse as UserMessage (which has a Text wrapper via FromJSON)
  case parseUserMessage inputValue of
    Left err -> pure $ Left $ "Invalid input for ExampleGraph: " <> err
    Right msg -> do
      let result = initializeWasm (runExampleGraph msg)
      output <- wasmResultToOutput result
      let session = ActiveSession
            { asGraphId = "example"
            , asStep = stepSession
            , asGetState = getSessionState
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


stepSession :: EffectResult -> IO StepOutput
stepSession effectResult = do
  st <- readIORef exampleState
  case st of
    ExampleIdle -> pure $ mkErrorOutput "ExampleGraph not initialized"
    ExampleWaiting resume _phase -> do
      let result = resume effectResult
      wasmResultToOutput result


getSessionState :: IO GraphState
getSessionState = do
  st <- readIORef exampleState
  pure $ case st of
    ExampleIdle -> GraphState PhaseIdle []
    ExampleWaiting _ phase -> GraphState phase []


wasmResultToOutput :: WasmResult Response -> IO StepOutput
wasmResultToOutput (WasmComplete a) = do
  writeIORef exampleState ExampleIdle
  pure $ exitToOutput a

wasmResultToOutput (WasmYield eff resume) = do
  let phase = PhaseInNode "classify"
  writeIORef exampleState (ExampleWaiting resume phase)
  pure $ StepYield eff (GraphState phase [])

wasmResultToOutput (WasmError msg) = do
  writeIORef exampleState ExampleIdle
  pure $ mkErrorOutput msg
