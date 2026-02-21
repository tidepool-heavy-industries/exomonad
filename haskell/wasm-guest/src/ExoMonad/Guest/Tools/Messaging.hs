-- | Agent↔TL messaging tool definitions.
--
-- Agent-side tools:
-- * 'Note' — fire-and-forget note from agent to TL
-- * 'Question' — blocking question from agent to TL (waits for answer)
--
-- TL-side tools:
-- * 'GetAgentMessages' — read notes/questions from agent outboxes
-- * 'AnswerQuestion' — answer a pending question from an agent
module ExoMonad.Guest.Tools.Messaging
  ( -- * Agent-side tool types
    Note,
    Question,

    -- * TL-side tool types
    GetAgentMessages,
    AnswerQuestion,

    -- * Argument types (exported for tests)
    NoteArgs (..),
    QuestionArgs (..),
    GetAgentMessagesArgs (..),
    AnswerQuestionArgs (..),
  )
where

import Data.Aeson (FromJSON, Value, object, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Vector qualified as V
import Data.Word (Word8)
import Effects.Messaging qualified as M
import ExoMonad.Effects.Messaging qualified as Messaging
import ExoMonad.Guest.Tool.Class
import ExoMonad.Guest.Tool.Schema (genericToolSchemaWith)
import GHC.Generics (Generic)
import Proto3.Suite.Class (fromByteString, toLazyByteString)

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
  toolDescription = "Send a fire-and-forget note to the TL (e.g. FYI updates, progress reports)."
  toolSchema =
    genericToolSchemaWith @NoteArgs
      [ ("content", "Note content to send to the TL")
      ]
  toolHandler args = do
    liftEffect (Messaging.sendNote (naContent args)) $ \_ ->
      object ["ack" .= True]

-- ============================================================================
-- Question
-- ============================================================================

-- | Blocking question from agent to TL. Waits for the TL's answer.
data Question

data QuestionArgs = QuestionArgs
  { qaContent :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON QuestionArgs where
  parseJSON = Aeson.withObject "QuestionArgs" $ \v ->
    QuestionArgs
      <$> v .: "content"

instance MCPTool Question where
  type ToolArgs Question = QuestionArgs
  toolName = "question"
  toolDescription = "Ask the TL a question and block until answered."
  toolSchema =
    genericToolSchemaWith @QuestionArgs
      [ ("content", "Question to ask the TL")
      ]
  toolHandler args = do
    liftEffect (Messaging.sendQuestion (qaContent args)) $ \resp ->
      object ["answer" .= TL.toStrict (M.sendQuestionResponseAnswer resp)]

  toolHandlerEff args = do
    let req =
          M.SendQuestionRequest
            { M.sendQuestionRequestQuestion = TL.fromStrict (qaContent args)
            }
        payloadBytes = BSL.unpack (toLazyByteString req)
    resultValue <- suspend (EffectRequest "messaging.send_question" (Aeson.toJSON payloadBytes))
    case Aeson.fromJSON resultValue of
      Aeson.Success (bytes :: [Word8]) ->
        case fromByteString (BS.pack bytes) of
          Right (resp :: M.SendQuestionResponse) ->
            pure $ successResult $ object ["answer" .= TL.toStrict (M.sendQuestionResponseAnswer resp)]
          Left err -> pure $ errorResult $ "Failed to decode response: " <> T.pack (show err)
      Aeson.Error err -> pure $ errorResult $ "Failed to parse result: " <> T.pack err

-- ============================================================================
-- GetAgentMessages (TL-side)
-- ============================================================================

-- | Read notes and pending questions from agent outboxes.
data GetAgentMessages

data GetAgentMessagesArgs = GetAgentMessagesArgs
  { gaAgentId :: Maybe Text,
    gaSubrepo :: Maybe Text,
    gaTimeoutSecs :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON GetAgentMessagesArgs where
  parseJSON = Aeson.withObject "GetAgentMessagesArgs" $ \v ->
    GetAgentMessagesArgs
      <$> v .:? "agent_id"
      <*> v .:? "subrepo"
      <*> v .:? "timeout_secs"

instance MCPTool GetAgentMessages where
  type ToolArgs GetAgentMessages = GetAgentMessagesArgs
  toolName = "get_agent_messages"
  toolDescription = "Read notes and pending questions from agent outboxes. Scans all agents (or a specific agent) for messages."
  toolSchema =
    genericToolSchemaWith @GetAgentMessagesArgs
      [ ("agent_id", "Filter to a specific agent directory name. If omitted, reads from all agents."),
        ("subrepo", "Subrepo path (e.g. 'egregore/') to scope agent scanning."),
        ("timeout_secs", "Long-poll timeout in seconds. 0 or omitted for immediate return.")
      ]
  toolHandler args = do
    let agentId = maybe "" id (gaAgentId args)
    let timeoutSecs = maybe 0 id (gaTimeoutSecs args)
    liftEffect (Messaging.getAgentMessages agentId timeoutSecs) $ \resp ->
      let agents = V.toList (M.getAgentMessagesResponseAgents resp)
          warning = TL.toStrict (M.getAgentMessagesResponseWarning resp)
          agentsJson = map agentMsgsToJson agents
       in object $
            ["agents" .= agentsJson]
              <> ["warning" .= warning | not (warning == "")]

agentMsgsToJson :: M.AgentMessages -> Value
agentMsgsToJson am =
  object
    [ "agent_id" .= TL.toStrict (M.agentMessagesAgentId am),
      "messages" .= map msgToJson (V.toList (M.agentMessagesMessages am))
    ]

msgToJson :: M.AgentMessage -> Value
msgToJson m =
  object
    [ "from" .= TL.toStrict (M.agentMessageFrom m),
      "text" .= TL.toStrict (M.agentMessageText m),
      "summary" .= TL.toStrict (M.agentMessageSummary m),
      "timestamp" .= TL.toStrict (M.agentMessageTimestamp m),
      "read" .= M.agentMessageRead m
    ]

-- ============================================================================
-- AnswerQuestion (TL-side)
-- ============================================================================

-- | Answer a pending question from an agent.
data AnswerQuestion

data AnswerQuestionArgs = AnswerQuestionArgs
  { aqAgentId :: Text,
    aqQuestionId :: Text,
    aqAnswer :: Text,
    aqSubrepo :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON AnswerQuestionArgs where
  parseJSON = Aeson.withObject "AnswerQuestionArgs" $ \v ->
    AnswerQuestionArgs
      <$> v .: "agent_id"
      <*> v .: "question_id"
      <*> v .: "answer"
      <*> v .:? "subrepo"

instance MCPTool AnswerQuestion where
  type ToolArgs AnswerQuestion = AnswerQuestionArgs
  toolName = "answer_question"
  toolDescription = "Answer a pending question from an agent. Writes the answer to the agent's inbox, unblocking their send_question call."
  toolSchema =
    genericToolSchemaWith @AnswerQuestionArgs
      [ ("agent_id", "The agent directory name (e.g. 'gh-42-fix-bug-claude')."),
        ("question_id", "The question ID to answer (e.g. 'q-abc123')."),
        ("answer", "The answer text."),
        ("subrepo", "Subrepo path (e.g. 'egregore/') if agent is in a subrepo.")
      ]
  toolHandler args = do

    liftEffect (Messaging.answerQuestion (aqAgentId args) (aqQuestionId args) (aqAnswer args)) $ \resp ->
      object
        [ "status" .= TL.toStrict (M.answerQuestionResponseStatus resp),
          "agent_id" .= TL.toStrict (M.answerQuestionResponseAgentId resp),
          "question_id" .= TL.toStrict (M.answerQuestionResponseQuestionId resp)
        ]

-- ============================================================================
-- Helpers
-- ============================================================================
