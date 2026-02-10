-- | TL-side messaging tool record.
--
-- Separate from 'MessagingTools' (agent-side note/question).
-- TL tools read agent outboxes and answer questions.
module ExoMonad.Guest.Records.TLMessaging
  ( TLMessagingTools (..),
    tlMessagingToolsHandler,
    tlMessagingToolsSchema,
    tlMessagingTools,
  )
where

import ExoMonad.Guest.Tool.Mode (AsHandler, AsSchema, ToolMode ((:-)), mkHandler, mkSchema)
import ExoMonad.Guest.Tools.Messaging (AnswerQuestion, GetAgentMessages)
import GHC.Generics (Generic)

-- | TL messaging tools record.
data TLMessagingTools mode = TLMessagingTools
  { getAgentMessages :: mode :- GetAgentMessages,
    answerQuestion :: mode :- AnswerQuestion
  }
  deriving (Generic)

-- | TL messaging tools handler record.
tlMessagingToolsHandler :: TLMessagingTools AsHandler
tlMessagingToolsHandler =
  TLMessagingTools
    { getAgentMessages = mkHandler @GetAgentMessages,
      answerQuestion = mkHandler @AnswerQuestion
    }

-- | TL messaging tools schema record.
tlMessagingToolsSchema :: TLMessagingTools AsSchema
tlMessagingToolsSchema =
  TLMessagingTools
    { getAgentMessages = mkSchema @GetAgentMessages,
      answerQuestion = mkSchema @AnswerQuestion
    }

-- | Default handler instance for use in Role.hs
tlMessagingTools :: TLMessagingTools AsHandler
tlMessagingTools = tlMessagingToolsHandler
