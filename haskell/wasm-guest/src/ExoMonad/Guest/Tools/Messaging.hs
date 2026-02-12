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
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Effects.Messaging qualified as M
import Data.Vector qualified as V
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
  toolDescription = "Send a fire-and-forget note to the TL (e.g. FYI updates, progress reports)."
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
    result <- Messaging.sendNote (naContent args)
    case result of
      Left err -> pure $ errorResult (T.pack (show err))
      Right _resp -> pure $ successResult $ object ["ack" .= True]

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
    object
      [ "type" .= ("object" :: Text),
        "required" .= (["content"] :: [Text]),
        "properties"
          .= object
            [ "content"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Question to ask the TL" :: Text)
                  ]
            ]
      ]
  toolHandler args = do
    result <- Messaging.sendQuestion (qaContent args)
    case result of
      Left err -> pure $ errorResult (T.pack (show err))
      Right resp -> pure $ successResult $ object ["answer" .= TL.toStrict (M.sendQuestionResponseAnswer resp)]

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
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "agent_id"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Filter to a specific agent directory name. If omitted, reads from all agents." :: Text)
                  ],
              "subrepo"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Subrepo path (e.g. 'egregore/') to scope agent scanning." :: Text)
                  ],
              "timeout_secs"
                .= object
                  [ "type" .= ("integer" :: Text),
                    "description" .= ("Long-poll timeout in seconds. 0 or omitted for immediate return." :: Text)
                  ]
            ]
      ]
  toolHandler args = do
    let agentId = maybe "" id (gaAgentId args)
    let timeoutSecs = maybe 0 id (gaTimeoutSecs args)
    result <- Messaging.getAgentMessages agentId timeoutSecs
    case result of
      Left err -> pure $ errorResult (T.pack (show err))
      Right resp ->
        let agents = V.toList (M.getAgentMessagesResponseAgents resp)
            warning = TL.toStrict (M.getAgentMessagesResponseWarning resp)
            agentsJson = map agentMsgsToJson agents
            resultJson = object $
              ["agents" .= agentsJson]
              <> ["warning" .= warning | not (warning == "")]
         in pure $ successResult resultJson

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
    object
      [ "type" .= ("object" :: Text),
        "required" .= (["agent_id", "question_id", "answer"] :: [Text]),
        "properties"
          .= object
            [ "agent_id"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The agent directory name (e.g. 'gh-42-fix-bug-claude')." :: Text)
                  ],
              "question_id"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The question ID to answer (e.g. 'q-abc123')." :: Text)
                  ],
              "answer"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The answer text." :: Text)
                  ],
              "subrepo"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Subrepo path (e.g. 'egregore/') if agent is in a subrepo." :: Text)
                  ]
            ]
      ]
  toolHandler args = do
    result <- Messaging.answerQuestion (aqAgentId args) (aqQuestionId args) (aqAnswer args)
    case result of
      Left err -> pure $ errorResult (T.pack (show err))
      Right resp ->
        pure $ successResult $
          object
            [ "status" .= TL.toStrict (M.answerQuestionResponseStatus resp),
              "agent_id" .= TL.toStrict (M.answerQuestionResponseAgentId resp),
              "question_id" .= TL.toStrict (M.answerQuestionResponseQuestionId resp)
            ]

-- ============================================================================
-- Helpers
-- ============================================================================

