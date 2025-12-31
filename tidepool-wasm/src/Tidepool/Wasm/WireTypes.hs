{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Wire types for WASM ↔ TypeScript communication.
--
-- These types cross the WASM/JSON boundary and must match the
-- TypeScript definitions in @deploy/src/protocol.ts@.
--
-- Design: Domain-specific effects only (LLM, Habitica, Log).
-- No general-purpose primitives like HTTP fetch.
module Tidepool.Wasm.WireTypes
  ( -- * Effects (WASM → TypeScript)
    SerializableEffect(..)

    -- * Results (TypeScript → WASM)
  , EffectResult(..)

    -- * Graph State (for observability)
  , ExecutionPhase(..)
  , GraphState(..)

    -- * Step Output
  , StepOutput(..)
  ) where

import Data.Aeson
  ( ToJSON(..)
  , FromJSON(..)
  , Value(..)
  , object
  , (.=)
  , (.:)
  , (.:?)
  , withObject
  )
import Data.Text (Text)
import GHC.Generics (Generic)


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECTS (WASM → TypeScript)
-- ════════════════════════════════════════════════════════════════════════════

-- | Effects that Haskell yields for TypeScript to execute.
--
-- JSON encoding matches protocol.ts: @{type: "LogInfo", eff_message: "..."}@
data SerializableEffect
  = EffLlmComplete
      { effNode :: Text
      -- ^ Which node is making this call
      , effSystemPrompt :: Text
      -- ^ System prompt
      , effUserContent :: Text
      -- ^ User content
      , effSchema :: Maybe Value
      -- ^ JSON schema for structured output
      }
  | EffLogInfo { effMessage :: Text }
  | EffLogError { effMessage :: Text }
  | EffHabitica
      { effHabOp :: Text
      -- ^ Operation name: "GetUser", "ScoreTask", "GetTasks", etc.
      , effHabPayload :: Value
      -- ^ Operation-specific payload (taskId, direction, etc.)
      }
  deriving stock (Show, Eq, Generic)

instance ToJSON SerializableEffect where
  toJSON (EffLlmComplete node sys user schema) = object $
    [ "type" .= ("LlmComplete" :: Text)
    , "eff_node" .= node
    , "eff_system_prompt" .= sys
    , "eff_user_content" .= user
    ] ++ maybe [] (\s -> ["eff_schema" .= s]) schema
  toJSON (EffLogInfo msg) = object
    [ "type" .= ("LogInfo" :: Text)
    , "eff_message" .= msg
    ]
  toJSON (EffLogError msg) = object
    [ "type" .= ("LogError" :: Text)
    , "eff_message" .= msg
    ]
  toJSON (EffHabitica op payload) = object
    [ "type" .= ("Habitica" :: Text)
    , "eff_hab_op" .= op
    , "eff_hab_payload" .= payload
    ]

instance FromJSON SerializableEffect where
  parseJSON = withObject "SerializableEffect" $ \o -> do
    (typ :: Text) <- o .: "type"
    case typ of
      "LlmComplete" -> EffLlmComplete
        <$> o .: "eff_node"
        <*> o .: "eff_system_prompt"
        <*> o .: "eff_user_content"
        <*> o .:? "eff_schema"
      "LogInfo" -> EffLogInfo <$> o .: "eff_message"
      "LogError" -> EffLogError <$> o .: "eff_message"
      "Habitica" -> EffHabitica
        <$> o .: "eff_hab_op"
        <*> o .: "eff_hab_payload"
      _         -> fail $ "Unknown effect type: " ++ show typ


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT RESULTS (TypeScript → WASM)
-- ════════════════════════════════════════════════════════════════════════════

-- | Result of effect execution from TypeScript.
--
-- For Log effects, TypeScript just acknowledges (success with null value).
-- For LLM effects (future), success contains parsed output.
--
-- JSON encoding: @{type: "success", value: ...}@ or @{type: "error", message: "..."}@
data EffectResult
  = ResSuccess { resValue :: Maybe Value }
  | ResError { resMessage :: Text }
  deriving stock (Show, Eq, Generic)

instance ToJSON EffectResult where
  toJSON (ResSuccess val) = object $
    [ "type" .= ("success" :: Text)
    ] ++ maybe [] (\v -> ["value" .= v]) val
  toJSON (ResError msg) = object
    [ "type" .= ("error" :: Text)
    , "message" .= msg
    ]

instance FromJSON EffectResult where
  parseJSON = withObject "EffectResult" $ \o -> do
    (typ :: Text) <- o .: "type"
    case typ of
      "success" -> ResSuccess <$> o .:? "value"
      "error"   -> ResError <$> o .: "message"
      _         -> fail $ "Unknown result type: " ++ show typ


-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH STATE (for observability)
-- ════════════════════════════════════════════════════════════════════════════

-- | Execution phase - matches protocol.ts ExecutionPhase exactly.
--
-- JSON encoding uses flat object with "type" discriminator:
-- - @{type: "idle"}@
-- - @{type: "in_node", nodeName: "classify"}@
-- - @{type: "transitioning", fromNode: "a", toNode: "b"}@
-- - @{type: "completed", result: ...}@
-- - @{type: "failed", error: "..."}@
data ExecutionPhase
  = PhaseIdle
  | PhaseInNode { phaseName :: Text }
  | PhaseTransitioning { phaseFrom :: Text, phaseTo :: Text }
  | PhaseCompleted { phaseResult :: Value }
  | PhaseFailed { phaseError :: Text }
  deriving stock (Show, Eq, Generic)

instance ToJSON ExecutionPhase where
  toJSON PhaseIdle = object
    [ "type" .= ("idle" :: Text)
    ]
  toJSON (PhaseInNode name) = object
    [ "type" .= ("in_node" :: Text)
    , "nodeName" .= name
    ]
  toJSON (PhaseTransitioning from to) = object
    [ "type" .= ("transitioning" :: Text)
    , "fromNode" .= from
    , "toNode" .= to
    ]
  toJSON (PhaseCompleted result) = object
    [ "type" .= ("completed" :: Text)
    , "result" .= result
    ]
  toJSON (PhaseFailed err) = object
    [ "type" .= ("failed" :: Text)
    , "error" .= err
    ]

instance FromJSON ExecutionPhase where
  parseJSON = withObject "ExecutionPhase" $ \o -> do
    (typ :: Text) <- o .: "type"
    case typ of
      "idle"          -> pure PhaseIdle
      "in_node"       -> PhaseInNode <$> o .: "nodeName"
      "transitioning" -> PhaseTransitioning <$> o .: "fromNode" <*> o .: "toNode"
      "completed"     -> PhaseCompleted <$> o .: "result"
      "failed"        -> PhaseFailed <$> o .: "error"
      _               -> fail $ "Unknown execution phase type: " ++ show typ


-- | Runtime graph state - matches protocol.ts GraphState.
--
-- JSON encoding: @{phase: {...}, completedNodes: [...]}@
data GraphState = GraphState
  { gsPhase :: ExecutionPhase
  -- ^ Current execution phase
  , gsCompletedNodes :: [Text]
  -- ^ Nodes that have completed
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON GraphState where
  toJSON gs = object
    [ "phase" .= gs.gsPhase
    , "completedNodes" .= gs.gsCompletedNodes
    ]

instance FromJSON GraphState where
  parseJSON = withObject "GraphState" $ \o -> GraphState
    <$> o .: "phase"
    <*> o .: "completedNodes"


-- ════════════════════════════════════════════════════════════════════════════
-- STEP OUTPUT (WASM → TypeScript per step)
-- ════════════════════════════════════════════════════════════════════════════

-- | Output from each step of graph execution.
--
-- This is a sum type that makes invalid states unrepresentable:
-- - 'StepYield': Not done, has an effect to execute
-- - 'StepDone': Completed successfully with a result
-- - 'StepFailed': Completed with an error
--
-- The execution loop:
-- 1. TypeScript calls @initialize(input)@ → gets StepOutput
-- 2. If StepYield: execute the effect, call @step(result)@ → gets StepOutput
-- 3. Repeat until StepDone or StepFailed
data StepOutput
  = StepYield
      { soEffect :: SerializableEffect
      -- ^ Effect to execute
      , soGraphState :: GraphState
      -- ^ Current graph execution state (for observability)
      }
  | StepDone
      { soResult :: Value
      -- ^ Final result (matches Exit type)
      , soGraphState :: GraphState
      -- ^ Final graph execution state
      }
  | StepFailed
      { soError :: Text
      -- ^ Error message
      , soGraphState :: GraphState
      -- ^ Graph state at failure
      }
  deriving stock (Show, Eq, Generic)

instance ToJSON StepOutput where
  toJSON (StepYield eff gs) = object
    [ "effect" .= Just eff
    , "done" .= False
    , "stepResult" .= Null
    , "graphState" .= gs
    ]
  toJSON (StepDone result gs) = object
    [ "effect" .= Null
    , "done" .= True
    , "stepResult" .= result
    , "graphState" .= gs
    ]
  toJSON (StepFailed err gs) = object
    [ "effect" .= Null
    , "done" .= True
    , "stepResult" .= Null
    , "error" .= err
    , "graphState" .= gs
    ]

instance FromJSON StepOutput where
  parseJSON = withObject "StepOutput" $ \o -> do
    done <- o .: "done"
    mEffect <- o .:? "effect"
    mError <- o .:? "error"
    gs <- o .: "graphState"
    case (done, mEffect, mError) of
      -- Valid: not done, has effect to execute
      (False, Just eff, _) -> pure $ StepYield eff gs
      -- Valid: done with error
      (True, Nothing, Just err) -> pure $ StepFailed err gs
      -- Valid: done with result
      (True, Nothing, Nothing) -> do
        result <- o .: "stepResult"
        pure $ StepDone result gs
      -- Invalid: done should not have an effect
      (True, Just _, _) ->
        fail "StepOutput: done=true should not have an effect"
      -- Invalid: not done requires an effect
      (False, Nothing, _) ->
        fail "StepOutput: done=false requires an effect"
