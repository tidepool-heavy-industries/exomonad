{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Messaging effects for agent↔TL communication.
--
-- All effects are dispatched via the @messaging@ namespace.
-- Request and response types are proto-generated from @proto/effects/messaging.proto@.
--
-- = Example
--
-- @
-- import ExoMonad.Effects.Messaging
--
-- main :: IO ()
-- main = do
--   _ <- sendNote (SendNoteRequest "FYI: found a bug in auth module")
--   answer <- sendQuestion (SendQuestionRequest "Should I fix auth or move on?" "")
--   pure ()
-- @
module ExoMonad.Effects.Messaging
  ( -- * Effect Types (Agent→TL)
    MessagingSendNote,
    MessagingSendQuestion,

    -- * Effect Types (TL→Agent)
    MessagingGetAgentMessages,
    MessagingAnswerQuestion,

    -- * Smart Constructors (Agent→TL)
    sendNote,
    sendQuestion,

    -- * Smart Constructors (TL→Agent)
    getAgentMessages,
    answerQuestion,

    -- * Re-exported proto types
    module Effects.Messaging,
  )
where

import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Effects.EffectError (EffectError)
import Effects.Messaging
import ExoMonad.Effect.Class (Effect (..), runEffect)

-- ============================================================================
-- Effect phantom types + instances (Agent→TL)
-- ============================================================================

data MessagingSendNote

instance Effect MessagingSendNote where
  type Input MessagingSendNote = SendNoteRequest
  type Output MessagingSendNote = SendNoteResponse
  effectId = "messaging.send_note"

data MessagingSendQuestion

instance Effect MessagingSendQuestion where
  type Input MessagingSendQuestion = SendQuestionRequest
  type Output MessagingSendQuestion = SendQuestionResponse
  effectId = "messaging.send_question"

-- ============================================================================
-- Effect phantom types + instances (TL→Agent)
-- ============================================================================

data MessagingGetAgentMessages

instance Effect MessagingGetAgentMessages where
  type Input MessagingGetAgentMessages = GetAgentMessagesRequest
  type Output MessagingGetAgentMessages = GetAgentMessagesResponse
  effectId = "messaging.get_agent_messages"

data MessagingAnswerQuestion

instance Effect MessagingAnswerQuestion where
  type Input MessagingAnswerQuestion = AnswerQuestionRequest
  type Output MessagingAnswerQuestion = AnswerQuestionResponse
  effectId = "messaging.answer_question"

-- ============================================================================
-- Smart constructors (Agent→TL)
-- ============================================================================

sendNote :: Text -> Text -> IO (Either EffectError SendNoteResponse)
sendNote content teamName =
  runEffect @MessagingSendNote $
    SendNoteRequest
      { sendNoteRequestContent = TL.fromStrict content,
        sendNoteRequestTeamName = TL.fromStrict teamName
      }

sendQuestion :: Text -> Text -> IO (Either EffectError SendQuestionResponse)
sendQuestion question teamName =
  runEffect @MessagingSendQuestion $
    SendQuestionRequest
      { sendQuestionRequestQuestion = TL.fromStrict question,
        sendQuestionRequestTeamName = TL.fromStrict teamName
      }

-- ============================================================================
-- Smart constructors (TL→Agent)
-- ============================================================================

-- | Read messages from agent outboxes. Empty agent_id reads all agents.
getAgentMessages :: Text -> Text -> IO (Either EffectError GetAgentMessagesResponse)
getAgentMessages agentId teamName =
  runEffect @MessagingGetAgentMessages $
    GetAgentMessagesRequest
      { getAgentMessagesRequestAgentId = TL.fromStrict agentId,
        getAgentMessagesRequestTeamName = TL.fromStrict teamName
      }

-- | Answer a pending question from an agent.
answerQuestion :: Text -> Text -> Text -> Text -> IO (Either EffectError AnswerQuestionResponse)
answerQuestion agentId questionId answer teamName =
  runEffect @MessagingAnswerQuestion $
    AnswerQuestionRequest
      { answerQuestionRequestAgentId = TL.fromStrict agentId,
        answerQuestionRequestQuestionId = TL.fromStrict questionId,
        answerQuestionRequestAnswer = TL.fromStrict answer,
        answerQuestionRequestTeamName = TL.fromStrict teamName
      }
