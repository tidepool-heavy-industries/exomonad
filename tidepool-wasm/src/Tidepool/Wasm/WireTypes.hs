{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Wire types for WASM ↔ TypeScript communication.
--
-- These types cross the WASM/JSON boundary and must match the
-- TypeScript definitions in @deploy/src/protocol.ts@.
--
-- Design: Start minimal - only what step 1 needs. Expand later
-- as we add LLM effects, HTTP, etc.
module Tidepool.Wasm.WireTypes
  ( -- * Effects (WASM → TypeScript)
    SerializableEffect(..)

    -- * Results (TypeScript → WASM)
  , EffectResult(..)

    -- * Step Output
  , StepOutput(..)
  ) where

import Data.Aeson
  ( ToJSON(..)
  , FromJSON(..)
  , Value
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
  | EffHttpFetch
      { effUrl :: Text
      -- ^ URL to fetch
      , effMethod :: Text
      -- ^ HTTP method
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
  toJSON (EffLlmComplete node sys user schema) = object
    [ "type" .= ("LlmComplete" :: Text)
    , "eff_node" .= node
    , "eff_system_prompt" .= sys
    , "eff_user_content" .= user
    , "eff_schema" .= schema
    ]
  toJSON (EffHttpFetch url method) = object
    [ "type" .= ("HttpFetch" :: Text)
    , "eff_url" .= url
    , "eff_method" .= method
    ]
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
      "HttpFetch" -> EffHttpFetch
        <$> o .: "eff_url"
        <*> o .: "eff_method"
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
  toJSON (ResSuccess val) = object
    [ "type" .= ("success" :: Text)
    , "value" .= val
    ]
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
-- STEP OUTPUT (WASM → TypeScript per step)
-- ════════════════════════════════════════════════════════════════════════════

-- | Output from each step of graph execution.
--
-- The execution loop:
-- 1. TypeScript calls @initialize(input)@ → gets StepOutput
-- 2. If effect present: execute it, call @step(result)@ → gets StepOutput
-- 3. Repeat until @done = True@
-- 4. Final result in @stepResult@
data StepOutput = StepOutput
  { soEffect     :: Maybe SerializableEffect
  -- ^ Effect to execute (Nothing if pure step or done)
  , soDone       :: Bool
  -- ^ Is graph execution complete?
  , soStepResult :: Maybe Value
  -- ^ Final result when done=True (matches Exit type)
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON StepOutput where
  toJSON so = object
    [ "effect" .= so.soEffect
    , "done" .= so.soDone
    , "stepResult" .= so.soStepResult
    ]

instance FromJSON StepOutput where
  parseJSON = withObject "StepOutput" $ \o -> StepOutput
    <$> o .:? "effect"
    <*> o .: "done"
    <*> o .:? "stepResult"
