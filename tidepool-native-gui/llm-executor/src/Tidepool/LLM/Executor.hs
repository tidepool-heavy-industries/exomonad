-- | LLM effect executor - Anthropic/OpenAI HTTP client.
--
-- Implements LLMComplete effect with native HTTP client.
-- Type-level provider determines request/response schema.
module Tidepool.LLM.Executor
  ( -- * Executor
    runLLM
  , LLMSecrets(..)
  ) where

import Data.Text (Text)

-- | LLM API secrets.
data LLMSecrets = LLMSecrets
  { lsAnthropicKey :: Maybe Text
  , lsOpenAIKey :: Maybe Text
  }

-- | Run LLM effects (stub - actual implementation by Agent 7).
runLLM :: LLMSecrets -> a -> a
runLLM _ = id  -- stub
