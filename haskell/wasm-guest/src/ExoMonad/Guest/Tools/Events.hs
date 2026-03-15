{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module ExoMonad.Guest.Tools.Events
  ( NotifyParent (..),
    SendMessage (..),
    Shutdown,
  )
where

import Control.Monad (void)
import Control.Monad.Freer (Eff)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Effects.Agent qualified as AgentProto
import Effects.Log qualified as Log
import ExoMonad.Effects.Agent qualified as ProtoAgent
import ExoMonad.Effects.Events qualified as ProtoEvents
import ExoMonad.Effects.Log (LogEmitEvent)
import ExoMonad.Guest.Lifecycle (DevPhase (..), setDevPhase)
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
    Aeson.Object $
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
  toolDescription = "Send a message to your parent agent. Use for status updates, progress reports, or failure escalation. Messages are delivered as-is with lightweight attribution. For PR-based workflows, the system auto-notifies your parent when Copilot approves — you don't need to signal completion yourself."
  toolSchema =
    genericToolSchemaWith @NotifyParentArgs
      [ ("status", "'success' = normal message (status update, progress report). 'failure' = escalation, something went wrong."),
        ("message", "The message to send. Be concise — one or two sentences."),
        ("pr_number", "PR number if relevant. Helps parent locate the PR without searching."),
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
      Right _ -> do
        -- Set terminal phase based on status
        case npStatus args of
          Success -> setDevPhase DevDone
          Failure -> setDevPhase (DevFailed (npMessage args))
        pure $ successResult $ object ["success" .= True]

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

-- | Send message tool
data SendMessage = SendMessage

data SendMessageArgs = SendMessageArgs
  { smRecipient :: Text,
    smContent :: Text,
    smSummary :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON SendMessageArgs where
  parseJSON = withObject "SendMessageArgs" $ \v ->
    SendMessageArgs
      <$> v .: "recipient"
      <*> v .: "content"
      <*> v .:? "summary"

instance ToJSON SendMessageArgs where
  toJSON args =
    object
      [ "recipient" .= smRecipient args,
        "content" .= smContent args,
        "summary" .= smSummary args
      ]

instance MCPTool SendMessage where
  type ToolArgs SendMessage = SendMessageArgs
  toolName = "send_message"
  toolDescription = "for sending messages to other exomonad-spawned agents (workers, leaves, subtrees) not Claude Code native teammates"
  toolSchema =
    genericToolSchemaWith @SendMessageArgs
      [ ("recipient", "The name of the agent to receive the message"),
        ("content", "The content of the message"),
        ("summary", "An optional summary of the message")
      ]
  toolHandlerEff args = do
    result <- suspendEffect @ProtoEvents.EventsSendMessage
                (ProtoEvents.SendMessageRequest
                  { ProtoEvents.sendMessageRequestRecipient = TL.fromStrict (smRecipient args),
                    ProtoEvents.sendMessageRequestContent = TL.fromStrict (smContent args),
                    ProtoEvents.sendMessageRequestSummary = maybe "" TL.fromStrict (smSummary args)
                  })
    case result of
      Left err -> pure $ errorResult (T.pack (show err))
      Right resp -> pure $ successResult $ object
        [ "success" .= ProtoEvents.sendMessageResponseSuccess resp,
          "delivery_method" .= ProtoEvents.sendMessageResponseDeliveryMethod resp
        ]

-- | Shutdown tool for cooperative agent exit
data Shutdown = Shutdown

data ShutdownArgs = ShutdownArgs
  { sdMessage :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON ShutdownArgs where
  parseJSON = withObject "ShutdownArgs" $ \v ->
    ShutdownArgs <$> v .:? "message"

instance ToJSON ShutdownArgs where
  toJSON args = object ["message" .= sdMessage args]

instance MCPTool Shutdown where
  type ToolArgs Shutdown = ShutdownArgs
  toolName = "shutdown"
  toolDescription = "Gracefully shut down this agent. Sends a final message to your parent, then exits. Call this when instructed to shut down or when your work is complete."
  toolSchema =
    genericToolSchemaWith @ShutdownArgs
      [("message", "Optional final message to send to parent before shutting down")]
  toolHandlerEff args = do
    let msg = maybe "Shutting down." id (sdMessage args)
    let statusText = "success" :: Text
    setDevPhase DevDone
    void $ suspendEffect @ProtoEvents.EventsNotifyParent
      (ProtoEvents.NotifyParentRequest
        { ProtoEvents.notifyParentRequestAgentId = "",
          ProtoEvents.notifyParentRequestStatus = TL.fromStrict statusText,
          ProtoEvents.notifyParentRequestMessage = TL.fromStrict msg
        })
    void $ suspendEffect @ProtoAgent.AgentCloseSelf
      (AgentProto.CloseSelfRequest
        { AgentProto.closeSelfRequestReason = TL.fromStrict msg })
    pure $ successResult $ object ["shutdown" .= True]
