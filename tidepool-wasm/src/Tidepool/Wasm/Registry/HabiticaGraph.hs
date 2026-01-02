{-# LANGUAGE OverloadedStrings #-}

-- | HabiticaRoutingGraph registry entry.
module Tidepool.Wasm.Registry.HabiticaGraph
  ( habiticaGraphEntry
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), Result(..), object, (.=), fromJSON)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)

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
-- SESSION STATE
-- ════════════════════════════════════════════════════════════════════════════

data HabiticaState
  = HabiticaIdle
  | HabiticaWaiting (EffectResult -> WasmResult ExecutionResult) ExecutionPhase

{-# NOINLINE habiticaState #-}
habiticaState :: IORef HabiticaState
habiticaState = unsafePerformIO $ newIORef HabiticaIdle


-- ════════════════════════════════════════════════════════════════════════════
-- SESSION CREATION
-- ════════════════════════════════════════════════════════════════════════════

exitToOutput :: ExecutionResult -> StepOutput
exitToOutput result =
  let resultVal = toJSON result
  in StepDone resultVal (GraphState (PhaseCompleted resultVal) ["executeAction"])


createSession :: Value -> IO (Either Text (StepOutput, ActiveSession))
createSession inputValue = do
  writeIORef habiticaState HabiticaIdle

  case fromJSON inputValue of
    Error err -> pure $ Left $ "Invalid input for HabiticaRoutingGraph: " <> T.pack err
    Success (input :: RawInput) -> do
      let result = initializeWasm (runHabiticaRoutingGraph input)
      output <- wasmResultToOutput result
      let session = ActiveSession
            { asGraphId = "habitica"
            , asStep = stepSession
            , asGetState = getSessionState
            , asGraphInfo = graphInfo
            }
      pure $ Right (output, session)


stepSession :: EffectResult -> IO StepOutput
stepSession effectResult = do
  st <- readIORef habiticaState
  case st of
    HabiticaIdle -> pure $ mkErrorOutput "HabiticaRoutingGraph not initialized"
    HabiticaWaiting resume _phase -> do
      let result = resume effectResult
      wasmResultToOutput result


getSessionState :: IO GraphState
getSessionState = do
  st <- readIORef habiticaState
  pure $ case st of
    HabiticaIdle -> GraphState PhaseIdle []
    HabiticaWaiting _ phase -> GraphState phase []


wasmResultToOutput :: WasmResult ExecutionResult -> IO StepOutput
wasmResultToOutput (WasmComplete a) = do
  writeIORef habiticaState HabiticaIdle
  pure $ exitToOutput a

wasmResultToOutput (WasmYield eff resume) = do
  let phase = PhaseInNode "extractTask"
  writeIORef habiticaState (HabiticaWaiting resume phase)
  pure $ StepYield eff (GraphState phase [])

wasmResultToOutput (WasmError msg) = do
  writeIORef habiticaState HabiticaIdle
  pure $ mkErrorOutput msg
