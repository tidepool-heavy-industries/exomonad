{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Worker role config: notify_parent + shutdown, allow-all hooks, no state transitions.
module WorkerRole (config, Tools) where

import Data.Aeson (object, (.=))
import ExoMonad
import ExoMonad.Guest.Tools.Events
  ( notifyParentCore, notifyParentDescription, notifyParentSchema, NotifyParentArgs,
    shutdownCore, shutdownDescription, shutdownSchema, ShutdownArgs
  )
import ExoMonad.Guest.Tools.Tasks (TaskList, TaskGet, TaskUpdate)
import ExoMonad.Guest.Types (allowResponse, allowStopResponse, postToolUseResponse)
import ExoMonad.Types (HookConfig (..), defaultSessionStartHook)

-- | Worker notify_parent: thin wrapper, no phase transitions.
data WorkerNotifyParent

instance MCPTool WorkerNotifyParent where
  type ToolArgs WorkerNotifyParent = NotifyParentArgs
  toolName = "notify_parent"
  toolDescription = notifyParentDescription
  toolSchema = notifyParentSchema
  toolHandlerEff args = do
    result <- notifyParentCore args
    case result of
      Left err -> pure $ errorResult err
      Right _ -> pure $ successResult $ object ["success" .= True]

-- | Worker shutdown: thin wrapper, no phase transitions.
data WorkerShutdown

instance MCPTool WorkerShutdown where
  type ToolArgs WorkerShutdown = ShutdownArgs
  toolName = "shutdown"
  toolDescription = shutdownDescription
  toolSchema = shutdownSchema
  toolHandlerEff = shutdownCore

data Tools mode = Tools
  { notifyParent :: mode :- WorkerNotifyParent,
    sendMessage :: mode :- SendMessage,
    shutdown :: mode :- WorkerShutdown,
    taskList :: mode :- TaskList,
    taskGet :: mode :- TaskGet,
    taskUpdate :: mode :- TaskUpdate
  }
  deriving (Generic)

config :: RoleConfig Tools
config =
  RoleConfig
    { roleName = "worker",
      tools =
        Tools
          { notifyParent = mkHandler @WorkerNotifyParent,
            sendMessage = mkHandler @SendMessage,
            shutdown = mkHandler @WorkerShutdown,
            taskList = mkHandler @TaskList,
            taskGet = mkHandler @TaskGet,
            taskUpdate = mkHandler @TaskUpdate
          },
      schemas =
        Tools
          { notifyParent = withDescription "Send results or status to your parent agent",
            sendMessage = withDescription "Send a message to another exomonad-spawned agent",
            shutdown = withDescription "Gracefully exit and close your pane",
            taskList = withDescription "Check pending tasks to find your next assignment",
            taskGet = withDescription "Read full task details and requirements before starting work",
            taskUpdate = withDescription "Update task status, claim ownership, or mark complete"
          },
      hooks =
        HookConfig
          { preToolUse = \_ -> pure (allowResponse Nothing),
            postToolUse = \_ -> pure (postToolUseResponse Nothing),
            onStop = \_ -> pure allowStopResponse,
            onSubagentStop = \_ -> pure allowStopResponse,
            onSessionStart = defaultSessionStartHook
          },
      eventHandlers = defaultEventHandlers
    }
