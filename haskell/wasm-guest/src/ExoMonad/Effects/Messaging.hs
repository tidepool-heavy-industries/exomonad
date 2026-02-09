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
  ( -- * Effect Types
    MessagingSendNote,
    MessagingSendQuestion,

    -- * Smart Constructors
    sendNote,
    sendQuestion,

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
-- Effect phantom types + instances
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
-- Smart constructors
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
