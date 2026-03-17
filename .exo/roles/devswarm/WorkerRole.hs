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
import ExoMonad.Guest.Tools.Tasks
  ( taskListCore, taskListDescription, taskListSchema, TaskListArgs,
    taskGetCore, taskGetDescription, taskGetSchema, TaskGetArgs,
    taskUpdateCore, taskUpdateDescription, taskUpdateSchema, TaskUpdateArgs
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

data WorkerTaskList

instance MCPTool WorkerTaskList where
  type ToolArgs WorkerTaskList = TaskListArgs
  toolName = "task_list"
  toolDescription = taskListDescription
  toolSchema = taskListSchema
  toolHandlerEff args = do
    result <- taskListCore args
    case result of
      Left err -> pure $ errorResult err
      Right output -> pure $ successResult output

data WorkerTaskGet

instance MCPTool WorkerTaskGet where
  type ToolArgs WorkerTaskGet = TaskGetArgs
  toolName = "task_get"
  toolDescription = taskGetDescription
  toolSchema = taskGetSchema
  toolHandlerEff args = do
    result <- taskGetCore args
    case result of
      Left err -> pure $ errorResult err
      Right output -> pure $ successResult output

data WorkerTaskUpdate

instance MCPTool WorkerTaskUpdate where
  type ToolArgs WorkerTaskUpdate = TaskUpdateArgs
  toolName = "task_update"
  toolDescription = taskUpdateDescription
  toolSchema = taskUpdateSchema
  toolHandlerEff args = do
    result <- taskUpdateCore args
    case result of
      Left err -> pure $ errorResult err
      Right output -> pure $ successResult output

data Tools mode = Tools
  { notifyParent :: mode :- WorkerNotifyParent,
    sendMessage :: mode :- SendMessage,
    shutdown :: mode :- WorkerShutdown,
    taskList :: mode :- WorkerTaskList,
    taskGet :: mode :- WorkerTaskGet,
    taskUpdate :: mode :- WorkerTaskUpdate
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
            shutdown = mkHandler @WorkerShutdown,
            taskList = mkHandler @WorkerTaskList,
            taskGet = mkHandler @WorkerTaskGet,
            taskUpdate = mkHandler @WorkerTaskUpdate
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
