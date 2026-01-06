{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Handlers for the graph.
--
-- Each node in the graph has a corresponding handler here.
module Template.Handlers
  ( simpleHandlers
  , formatHistory
  ) where

import Data.Proxy (Proxy(..))
import qualified Data.Text as T

import Tidepool.Effect.Types (ChatHistory, getHistory, Message(..), ContentBlock(..), Role(..))
import Tidepool.Graph.Generic (AsHandler)
import Tidepool.Graph.Goto (LLMHandler(..), gotoExit)

import Template.Context (ProcessContext(..), HistoryMessage(HistoryMessage))
import Template.Graph (SimpleGraph(..), Input(..), Output(..), Result(..))
import Template.Templates (ProcessTpl)
import Tidepool.Graph.Template (templateCompiled)

-- | Handlers for SimpleGraph.
--
-- - sgProcess: LLMHandler builds template context and routes to exit
--
-- Uses ChatHistory effect to include conversation history in context.
simpleHandlers :: SimpleGraph (AsHandler '[ChatHistory])
simpleHandlers = SimpleGraph
  { sgEntry   = Proxy @Input

    -- LLMHandler: Build template context, call LLM, then route to exit
  , sgProcess = LLMHandler
      { llmSystem = Nothing
      , llmUser   = templateCompiled @ProcessTpl
      , llmBefore = \input -> do
          msgs <- getHistory
          pure ProcessContext
            { input = T.pack input.inputText
            , history = formatHistory msgs
            }
      , llmAfter  = \output -> pure $ gotoExit (Result output.outputText)
      }

  , sgExit    = Proxy @Result
  }

-- | Format raw Message list into simplified HistoryMessage list for templates.
--
-- Extracts only text content from messages, discarding images, tool use,
-- thinking blocks, etc. This provides a clean interface for templates.
formatHistory :: [Message] -> [HistoryMessage]
formatHistory = map formatMessage
  where
    formatMessage :: Message -> HistoryMessage
    formatMessage msg = HistoryMessage (roleToText msg.role) (extractTextContent msg.content)

    roleToText :: Role -> T.Text
    roleToText User = "user"
    roleToText Assistant = "assistant"

    extractTextContent :: [ContentBlock] -> T.Text
    extractTextContent blocks = T.intercalate "\n" $ concatMap extractText blocks

    extractText :: ContentBlock -> [T.Text]
    extractText (TextBlock t) = [t]
    extractText (JsonBlock _) = []  -- Skip structured output
    extractText (ImageBlock _) = []  -- Skip images
    extractText (ToolUseBlock _) = []  -- Skip tool invocations
    extractText (ToolResultBlock _) = []  -- Skip tool results
    extractText (ThinkingBlock _) = []  -- Skip thinking
    extractText (RedactedThinkingBlock _) = []  -- Skip redacted thinking
