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
  , ccMaxTokens = cfg.ccMaxTokens
  , ccTools = Just t
  }
