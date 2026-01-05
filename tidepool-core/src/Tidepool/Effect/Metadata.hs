{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Effect metadata for routing decisions.
--
-- This module defines metadata for all effect types (Log, LLM, State, etc.).
-- It's used by executors to determine how to route and schedule effects.
--
-- = Adding a New Effect
--
-- When adding a new effect GADT (e.g., data Foo r where ...):
--
-- 1. Define the effect type in Tidepool.Effect.Types
-- 2. Add a corresponding EffectMeta entry to 'allEffectMeta' here
-- 3. Decide: Internal (handled by framework) or Yielded (sent back to caller)
-- 4. Decide: FireAndForget or Blocking semantics
module Tidepool.Effect.Metadata
  ( -- * Effect Categories
    EffectCategory(..)
  , EffectSemantics(..)

    -- * Effect Metadata
  , EffectMeta(..)
  , allEffectMeta

    -- * Helpers
  , categoryToText
  , semanticsToText
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), withText)
import Data.Text (Text)
import GHC.Generics (Generic)


-- | Where an effect should be handled.
--
-- - 'Internal': Handled by StateMachineDO (LLM, Habitica, Log)
-- - 'Yielded': Sent back to caller for handling (Telegram effects → TelegramDO)
data EffectCategory
  = Internal
  | Yielded
  deriving stock (Show, Eq, Generic)

instance ToJSON EffectCategory where
  toJSON Internal = "internal"
  toJSON Yielded  = "yielded"

instance FromJSON EffectCategory where
  parseJSON = withText "EffectCategory" $ \t -> case t of
    "internal" -> pure Internal
    "yielded"  -> pure Yielded
    _          -> fail $ "Unknown EffectCategory: " ++ show t

-- | How an effect interacts with the execution loop.
--
-- - 'FireAndForget': Effect is executed but result is ignored (Log effects)
-- - 'Blocking': Execution waits for effect result before continuing
data EffectSemantics
  = FireAndForget
  | Blocking
  deriving stock (Show, Eq, Generic)

instance ToJSON EffectSemantics where
  toJSON FireAndForget = "fire_and_forget"
  toJSON Blocking      = "blocking"

instance FromJSON EffectSemantics where
  parseJSON = withText "EffectSemantics" $ \t -> case t of
    "fire_and_forget" -> pure FireAndForget
    "blocking"        -> pure Blocking
    _                 -> fail $ "Unknown EffectSemantics: " ++ show t


-- | Metadata for a single effect type.
data EffectMeta = EffectMeta
  { emTypeName  :: Text            -- ^ Effect type name (e.g., "LogInfo", "LlmComplete")
  , emCategory  :: EffectCategory  -- ^ Internal or Yielded
  , emSemantics :: EffectSemantics -- ^ FireAndForget or Blocking
  }
  deriving stock (Show, Eq, Generic)


-- | All effect metadata - SINGLE SOURCE OF TRUTH.
--
-- This list defines routing behavior for every effect type:
-- - Internal effects are handled by StateMachineDO
-- - Yielded effects are sent back to the caller
--
-- Keep in sync with effect types in Tidepool.Effect.Types!
-- For each effect GADT (State, LLM, Random, etc.), add one entry here.
allEffectMeta :: [EffectMeta]
allEffectMeta =
  -- Core IO-blind effects
  [ EffectMeta "Log"             Internal FireAndForget  -- LogMsg with various levels
  , EffectMeta "LLM"             Internal Blocking       -- RunTurnOp (tool-aware LLM calls)
  , EffectMeta "State"           Internal Blocking       -- Get/Put for agent state
  , EffectMeta "Random"          Internal Blocking       -- RandomInt/RandomDouble
  , EffectMeta "Time"            Internal Blocking       -- GetCurrentTime
  , EffectMeta "ChatHistory"     Internal Blocking       -- GetHistory/AppendMessages
  , EffectMeta "Emit"            Yielded  FireAndForget  -- EmitEvent for observability
  , EffectMeta "RequestInput"    Yielded  Blocking       -- RequestChoice/RequestText (user input)
  , EffectMeta "QuestionUI"      Yielded  Blocking       -- AskQuestion (structured questions)
  ]


-- | Convert EffectCategory to Text for codegen.
categoryToText :: EffectCategory -> Text
categoryToText Internal = "internal"
categoryToText Yielded  = "yielded"

-- | Convert EffectSemantics to Text for codegen.
semanticsToText :: EffectSemantics -> Text
semanticsToText FireAndForget = "fire_and_forget"
semanticsToText Blocking      = "blocking"
