-- | Builder pattern for LLM call configuration.
--
-- Provides ergonomic construction of 'CallConfig' values using
-- function composition.
--
-- = Usage
--
-- @
-- -- Basic config with defaults
-- let cfg = defaultLLM \@Report
--
-- -- Customized config
-- let cfg = defaultLLM \@Report
--       & model Opus
--       & temp 0.7
--       & maxTokens 4096
--
-- -- With tools
-- let cfg = defaultLLM \@Report
--       & model Opus
--       & tools MyTools { search = searchHandler }
-- @
module ExoMonad.LLM.Builder
  ( -- * Default Config
    defaultLLM

    -- * Builder Functions
  , model
  , temp
  , maxTokens
  , tools
  ) where

import ExoMonad.LLM.Types


-- ════════════════════════════════════════════════════════════════════════════
-- DEFAULT CONFIG
-- ════════════════════════════════════════════════════════════════════════════

-- | Create a default LLM config for the given output type.
--
-- Defaults:
--
-- * Model: Sonnet (balanced performance)
-- * Temperature: None (use provider default)
-- * Max tokens: None (use provider default)
-- * Tools: None
--
-- The output type is specified via TypeApplications:
--
-- @
-- let cfg = defaultLLM \@MyOutputType
-- @
defaultLLM :: forall out. CallConfig out NoTools
defaultLLM = CallConfig
  { ccModel = Sonnet
  , ccTemperature = Nothing
  , ccMaxTokens = Nothing
  , ccTools = Nothing
  }


-- ════════════════════════════════════════════════════════════════════════════
-- BUILDER FUNCTIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Set the model to use.
--
-- @
-- let cfg = defaultLLM \@Report & model Opus
-- @
model :: Model -> CallConfig out tools -> CallConfig out tools
model m cfg = cfg { ccModel = m }

-- | Set the temperature.
--
-- Temperature controls randomness in the output:
--
-- * 0.0 - Deterministic (most likely tokens)
-- * 1.0 - More creative/varied
--
-- @
-- let cfg = defaultLLM \@Report & temp 0.7
-- @
temp :: Double -> CallConfig out tools -> CallConfig out tools
temp t cfg = cfg { ccTemperature = Just t }

-- | Set the maximum tokens to generate.
--
-- @
-- let cfg = defaultLLM \@Report & maxTokens 4096
-- @
maxTokens :: Int -> CallConfig out tools -> CallConfig out tools
maxTokens n cfg = cfg { ccMaxTokens = Just n }

-- | Attach tools to the config.
--
-- This changes the type parameter from the current tool type to the
-- new tool type. The tools record contains handler functions that
-- can capture local context.
--
-- @
-- let cfg = defaultLLM \@Report
--       & tools MyTools
--           { search = \\args -> searchIn doc args
--           , readSection = \\args -> pure (doc.sections !! args.index)
--           }
-- @
--
-- Note: Adding tools changes the config's tool type parameter,
-- so you can only add tools once (or replace them entirely).
tools :: tools' -> CallConfig out tools -> CallConfig out tools'
tools t cfg = CallConfig
  { ccModel = cfg.ccModel
  , ccTemperature = cfg.ccTemperature
  , ccMaxTokens = cfg.ccMaxTokens
  , ccTools = Just t
  }
