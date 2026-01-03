{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Effect metadata for routing decisions.
--
-- This module is the SINGLE SOURCE OF TRUTH for effect routing metadata.
-- Both the Haskell runtime (WireTypes.hs) and TypeScript codegen (GraphSpecs.hs)
-- derive their routing logic from this module.
--
-- = Adding a New Effect
--
-- When adding a new effect type to SerializableEffect:
--
-- 1. Add the constructor to SerializableEffect in WireTypes.hs
-- 2. Add an entry to 'allEffectMeta' here
-- 3. The rest is automatic - codegen and runtime both use this list
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
-- Keep in sync with SerializableEffect constructors in WireTypes.hs!
allEffectMeta :: [EffectMeta]
allEffectMeta =
  [ EffectMeta "LogInfo"         Internal FireAndForget
  , EffectMeta "LogError"        Internal FireAndForget
  , EffectMeta "LlmComplete"     Internal Blocking
  , EffectMeta "LlmCall"         Internal Blocking      -- Tool-aware LLM calls
  , EffectMeta "Habitica"        Internal Blocking
  , EffectMeta "TelegramSend"    Yielded  FireAndForget
  , EffectMeta "TelegramAsk"     Yielded  Blocking
  ]


-- | Convert EffectCategory to Text for codegen.
categoryToText :: EffectCategory -> Text
categoryToText Internal = "internal"
categoryToText Yielded  = "yielded"

-- | Convert EffectSemantics to Text for codegen.
semanticsToText :: EffectSemantics -> Text
semanticsToText FireAndForget = "fire_and_forget"
semanticsToText Blocking      = "blocking"
