{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module ExoMonad.Guest.Tools.Events
  ( NotifyParent (..),
  )
where

import Control.Monad (void)
import Control.Monad.Freer (Eff, sendM)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Effects.Log qualified as Log
import ExoMonad.Effects.Events qualified as ProtoEvents
import ExoMonad.Effects.Log (LogEmitEvent)
import ExoMonad.Guest.Tool.Class (MCPCallOutput, MCPTool (..), errorResult, successResult)
import ExoMonad.Guest.Tool.Schema (JsonSchema (..), genericToolSchemaWith)
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect, suspendEffect_)
import GHC.Generics (Generic)

-- | Notify parent tool (for workers/subtrees to call on completion)
data NotifyParent = NotifyParent

-- | Status for notify_parent tool.
data NotifyStatus = Success | Failure
  deriving (Show, Eq, Generic, JsonSchema)

instance FromJSON NotifyStatus where
  parseJSON = Aeson.withText "NotifyStatus" $ \case
    "success" -> pure Success
    "failure" -> pure Failure
    other -> fail $ "Unknown status: " <> T.unpack other

instance ToJSON NotifyStatus where
  toJSON Success = Aeson.String "success"
  toJSON Failure = Aeson.String "failure"

-- | Structured task report for enriched notifications.
data TaskReport = TaskReport
  { trWhat :: Text,
    trHow :: Text
  }
  deriving (Generic, Show)

instance JsonSchema TaskReport where
  toSchema =
    genericToolSchemaWith @TaskReport
      [ ("what", "task description"),
        ("how", "verification command that was run")
      ]

instance FromJSON TaskReport where
  parseJSON = withObject "TaskReport" $ \v ->
    TaskReport <$> v .: "what" <*> v .: "how"

instance ToJSON TaskReport where
  toJSON (TaskReport w h) = object ["what" .= w, "how" .= h]

data NotifyParentArgs = NotifyParentArgs
  { npStatus :: NotifyStatus,
    npMessage :: Text,
    npPrNumber :: Maybe Int,
    npTasksCompleted :: Maybe [TaskReport]
  }
  deriving (Generic, Show)

instance FromJSON NotifyParentArgs where
  parseJSON = withObject "NotifyParentArgs" $ \v ->
    NotifyParentArgs
      <$> v .: "status"
      <*> v .: "message"
      <*> v .:? "pr_number"
      <*> v .:? "tasks_completed"

instance ToJSON NotifyParentArgs where
  toJSON args =
    object
      [ "status" .= npStatus args,
        "message" .= npMessage args,
        "pr_number" .= npPrNumber args,
        "tasks_completed" .= npTasksCompleted args
      ]

instance MCPTool NotifyParent where
  type ToolArgs NotifyParent = NotifyParentArgs
  toolName = "notify_parent"
  toolDescription = "Signal to your parent that you are DONE. Call as your final action — after PR is filed, Copilot feedback addressed, and changes pushed. Status 'success' means work is review-clean. Status 'failure' means retries exhausted, escalating to parent."
  toolSchema =
    genericToolSchemaWith @NotifyParentArgs
      [ ("status", "'success' = work is done and review-clean. 'failure' = exhausted retries, escalating to parent."),
        ("message", "One-line summary. On success: what was accomplished. On failure: what went wrong."),
        ("pr_number", "PR number if one was filed. Enables parent to immediately merge without searching."),
        ("tasks_completed", "Array of {what, how} pairs. 'what' = task description, 'how' = verification command that was run.")
      ]
  toolHandlerEff args = do
    -- Emit event via suspend
    let eventPayload = BSL.toStrict $ Aeson.encode $ object
          [ "status" .= npStatus args,
            "message" .= npMessage args,
            "pr_number" .= npPrNumber args,
            "tasks_completed" .= npTasksCompleted args
          ]
    void $ suspendEffect_ @LogEmitEvent (Log.EmitEventRequest
      { Log.emitEventRequestEventType = "agent.completed",
        Log.emitEventRequestPayload = eventPayload,
        Log.emitEventRequestTimestamp = 0
      })

    let richMessage = composeNotifyMessage args
    let statusText = case npStatus args of
          Success -> "success" :: Text
          Failure -> "failure"
    result <- suspendEffect @ProtoEvents.EventsNotifyParent
                (ProtoEvents.NotifyParentRequest
                  { ProtoEvents.notifyParentRequestAgentId = "",
                    ProtoEvents.notifyParentRequestStatus = TL.fromStrict statusText,
                    ProtoEvents.notifyParentRequestMessage = TL.fromStrict richMessage
                  })
    case result of
      Left err -> pure $ errorResult (T.pack (show err))
      Right _ -> pure $ successResult $ object ["success" .= True]

-- | Compose enriched notification message with PR number and task reports.
composeNotifyMessage :: NotifyParentArgs -> Text
composeNotifyMessage args =
  let base = npMessage args
      prSuffix = case npPrNumber args of
        Just n -> " (PR #" <> T.pack (show n) <> ")"
        Nothing -> ""
      taskLines = case npTasksCompleted args of
        Just tasks -> T.concat ["\n  - " <> trWhat t <> " (verified: " <> trHow t <> ")" | t <- tasks]
        Nothing -> ""
   in base <> prSuffix <> taskLines
