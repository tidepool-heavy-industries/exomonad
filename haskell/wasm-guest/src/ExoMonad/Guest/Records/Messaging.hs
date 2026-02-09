-- | Messaging tool record and handlers.
--
-- Tools for agent↔TL communication.
module ExoMonad.Guest.Records.Messaging
  ( MessagingTools (..),
    messagingToolsHandler,
    messagingToolsSchema,
    messagingTools,
  )
where

import ExoMonad.Guest.Tool.Mode (AsHandler, AsSchema, ToolMode ((:-)), mkHandler, mkSchema)
import ExoMonad.Guest.Tools.Messaging (Note, Question)
import GHC.Generics (Generic)

-- | Messaging tools record.
data MessagingTools mode = MessagingTools
  { note :: mode :- Note,
    question :: mode :- Question
  }
  deriving (Generic)

-- | Messaging tools handler record.
messagingToolsHandler :: MessagingTools AsHandler
messagingToolsHandler =
  MessagingTools
    { note = mkHandler @Note,
      question = mkHandler @Question
    }

-- | Messaging tools schema record.
messagingToolsSchema :: MessagingTools AsSchema
messagingToolsSchema =
  MessagingTools
    { note = mkSchema @Note,
      question = mkSchema @Question
    }

-- | Default handler instance for use in Role.hs
messagingTools :: MessagingTools AsHandler
messagingTools = messagingToolsHandler
