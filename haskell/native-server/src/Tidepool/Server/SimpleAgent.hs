-- | Simple LLM agent demonstrating full effect stack.
--
-- This agent uses:
--
-- * 'UI' - for user interaction (showText, requestTextInput)
-- * 'LLMComplete' - for calling Anthropic Claude
-- * 'Observability' - for logging events to Loki
--
-- It does NOT use Habitica (which is optional in the stack).
--
-- = Usage
--
-- @
-- import Tidepool.Server.SimpleAgent (simpleAgent)
-- import Tidepool.Server.EffectRunner (runEffects)
--
-- -- In WebSocket handler:
-- runEffects env ctx callback simpleAgent
-- @
module Tidepool.Server.SimpleAgent
  ( simpleAgent
  ) where

import Control.Monad (forever)
import Control.Monad.Freer (Eff, Member)
import Data.Text (Text)
import qualified Data.Text as T

import Tidepool.Effects.UI (UI, showText, requestTextInput)
import Tidepool.Effects.LLMProvider
  ( LLMComplete, complete, SProvider(..), AnthropicConfig(..)
  , AnthropicResponse(..), ContentBlock(..), ThinkingBudget(..)
  )
import Tidepool.Effects.Observability (Observability, publishEvent, TidepoolEvent(..))


-- | Default Anthropic config for the simple agent.
defaultConfig :: AnthropicConfig
defaultConfig = AnthropicConfig
  { acModel = "claude-sonnet-4-20250514"
  , acMaxTokens = 1024
  , acThinking = ThinkingDisabled
  , acSystemPrompt = Just "You are a helpful assistant. Be concise."
  }

-- | Extract text from Anthropic response.
extractText :: AnthropicResponse -> Text
extractText resp = T.intercalate "\n" [t | TextContent t <- resp.arContent]

-- | Simple agent that takes user input and gets LLM response.
--
-- This demonstrates the full effect composition working:
--
-- 1. Publishes observability events (Loki)
-- 2. Shows welcome message (UI)
-- 3. Loops: gets user input, calls LLM, shows response
simpleAgent :: (Member UI effs, Member LLMComplete effs, Member Observability effs)
            => Eff effs ()
simpleAgent = do
  publishEvent $ GraphTransition "entry" "greeting" "start"
  showText "Welcome! I'm a simple LLM agent. Ask me anything."
  chatLoop

-- | The main chat loop.
chatLoop :: (Member UI effs, Member LLMComplete effs, Member Observability effs)
         => Eff effs ()
chatLoop = forever $ do
  input <- requestTextInput "You:"
  publishEvent $ GraphTransition "chat" "llm_call" "user_input"

  -- Call LLM via Anthropic
  response <- complete SAnthropic defaultConfig input Nothing
  let responseText = extractText response

  publishEvent $ GraphTransition "llm_call" "chat" "response"
  showText responseText
