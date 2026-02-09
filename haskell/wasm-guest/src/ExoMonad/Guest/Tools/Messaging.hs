-- | Agent↔TL messaging tool definitions.
--
-- Two tools for inter-agent communication:
-- * 'Note' — fire-and-forget note from agent to TL
-- * 'Question' — blocking question from agent to TL (waits for answer)
module ExoMonad.Guest.Tools.Messaging
  ( -- * Tool types
    Note,
    Question,

    -- * Argument types (exported for tests)
    NoteArgs (..),
    QuestionArgs (..),
  )
where

import Data.Aeson (FromJSON, Value, object, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Effects.Messaging qualified as M
import ExoMonad.Effects.Messaging qualified as Messaging
import ExoMonad.Guest.Tool.Class
import GHC.Generics (Generic)

-- ============================================================================
-- Note
-- ============================================================================

-- | Fire-and-forget note from agent to TL.
data Note

data NoteArgs = NoteArgs
  { naContent :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON NoteArgs where
  parseJSON = Aeson.withObject "NoteArgs" $ \v ->
    NoteArgs
      <$> v .: "content"

instance MCPTool Note where
  type ToolArgs Note = NoteArgs
  toolName = "note"
  toolDescription = "Send a fire-and-forget note to the TL (e.g. FYI updates, progress reports)"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "required" .= (["content"] :: [Text]),
        "properties"
          .= object
            [ "content"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Note content to send to the TL" :: Text)
                  ]
            ]
      ]
  toolHandler args = do
    let req = M.SendNoteRequest {M.sendNoteRequestContent = TL.fromStrict (naContent args)}
    result <- Messaging.sendNote req
    case result of
      Left err -> pure $ errorResult (TL.toStrict $ "Messaging effect failed: " <> TL.pack (show err))
      Right _resp -> pure $ successResult $ object ["ack" .= True]

-- ============================================================================
-- Question
-- ============================================================================

-- | Blocking question from agent to TL. Waits for the TL's answer.
data Question

data QuestionArgs = QuestionArgs
  { qaContent :: Text,
    qaContext :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON QuestionArgs where
  parseJSON = Aeson.withObject "QuestionArgs" $ \v ->
    QuestionArgs
      <$> v .: "content"
      <*> v .:? "context"

instance MCPTool Question where
  type ToolArgs Question = QuestionArgs
  toolName = "question"
  toolDescription = "Ask the TL a question and block until answered"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "required" .= (["content"] :: [Text]),
        "properties"
          .= object
            [ "content"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Question to ask the TL" :: Text)
                  ],
              "context"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Optional additional context for the TL" :: Text)
                  ]
            ]
      ]
  toolHandler args = do
    let req =
          M.SendQuestionRequest
            { M.sendQuestionRequestContent = TL.fromStrict (qaContent args),
              M.sendQuestionRequestContext = maybe "" TL.fromStrict (qaContext args)
            }
    result <- Messaging.sendQuestion req
    case result of
      Left err -> pure $ errorResult (TL.toStrict $ "Messaging effect failed: " <> TL.pack (show err))
      Right resp -> pure $ successResult $ object ["answer" .= TL.toStrict (M.sendQuestionResponseAnswer resp)]
