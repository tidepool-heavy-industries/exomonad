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
    shutdown :: mode :- WorkerShutdown
  }
  deriving (Generic)

config :: RoleConfig (Tools AsHandler)
config =
  RoleConfig
    { roleName = "worker",
      tools =
        Tools
          { notifyParent = mkHandler @WorkerNotifyParent,
            sendMessage = mkHandler @SendMessage,
            shutdown = mkHandler @WorkerShutdown
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
