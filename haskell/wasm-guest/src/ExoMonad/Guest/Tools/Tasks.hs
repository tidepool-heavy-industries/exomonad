{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ExoMonad.Guest.Tools.Tasks
  ( TaskList, TaskListArgs(..), taskListCore, taskListDescription, taskListSchema,
    TaskGet, TaskGetArgs(..), taskGetCore, taskGetDescription, taskGetSchema,
    TaskUpdate, TaskUpdateArgs(..), taskUpdateCore, taskUpdateDescription, taskUpdateSchema,
  )
where

import Control.Monad (void)
import Control.Monad.Freer (Eff)
import Data.Aeson (FromJSON, ToJSON, object, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Effects.Tasks qualified as Tasks
import Effects.Log qualified as Log
import ExoMonad.Effects.Tasks (TasksListTasks, TasksGetTask, TasksUpdateTask)
import ExoMonad.Effects.Log (LogInfo)
import ExoMonad.Guest.Tool.Class (MCPTool(..), MCPCallOutput, successResult, errorResult)
import ExoMonad.Guest.Tool.Schema (genericToolSchemaWith)
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect, suspendEffect_)
import ExoMonad.Guest.Types (Effects)
import GHC.Generics (Generic)

-- TaskList
data TaskList

data TaskListArgs = TaskListArgs
  { tlStatusFilter :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON TaskListArgs where
  parseJSON = withObject "TaskListArgs" $ \v ->
    TaskListArgs
      <$> v .:? "status_filter"

taskListDescription :: Text
taskListDescription = "List all tasks in the shared task list. Optionally filter by status (pending, in_progress, completed)."

taskListSchema :: Aeson.Object
taskListSchema = genericToolSchemaWith @TaskListArgs
  [ ("status_filter", "Filter by status: pending, in_progress, completed. Empty for all.")
  ]

taskListCore :: TaskListArgs -> Eff Effects (Either Text Aeson.Value)
taskListCore args = do
  let req = Tasks.ListTasksRequest
        { Tasks.listTasksRequestTeamName = "",
          Tasks.listTasksRequestStatusFilter = maybe "" TL.fromStrict (tlStatusFilter args)
        }
  void $ suspendEffect_ @LogInfo (Log.InfoRequest {Log.infoRequestMessage = "TaskList: Calling list_tasks effect", Log.infoRequestFields = ""})
  result <- suspendEffect @TasksListTasks req
  case result of
    Left err -> pure $ Left (T.pack (show err))
    Right resp -> pure $ Right (Aeson.toJSON resp)

-- TaskGet
data TaskGet

data TaskGetArgs = TaskGetArgs
  { tgTaskId :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON TaskGetArgs where
  parseJSON = withObject "TaskGetArgs" $ \v ->
    TaskGetArgs
      <$> v .: "task_id"

taskGetDescription :: Text
taskGetDescription = "Get a task by ID from the shared task list."

taskGetSchema :: Aeson.Object
taskGetSchema = genericToolSchemaWith @TaskGetArgs
  [ ("task_id", "The task ID to retrieve")
  ]

taskGetCore :: TaskGetArgs -> Eff Effects (Either Text Aeson.Value)
taskGetCore args = do
  let req = Tasks.GetTaskRequest
        { Tasks.getTaskRequestTeamName = "",
          Tasks.getTaskRequestTaskId = TL.fromStrict (tgTaskId args)
        }
  void $ suspendEffect_ @LogInfo (Log.InfoRequest {Log.infoRequestMessage = "TaskGet: Calling get_task effect", Log.infoRequestFields = ""})
  result <- suspendEffect @TasksGetTask req
  case result of
    Left err -> pure $ Left (T.pack (show err))
    Right resp -> pure $ Right (Aeson.toJSON resp)

-- TaskUpdate
data TaskUpdate

data TaskUpdateArgs = TaskUpdateArgs
  { tuTaskId :: Text,
    tuStatus :: Maybe Text,
    tuOwner :: Maybe Text,
    tuActiveForm :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON TaskUpdateArgs where
  parseJSON = withObject "TaskUpdateArgs" $ \v ->
    TaskUpdateArgs
      <$> v .: "task_id"
      <*> v .:? "status"
      <*> v .:? "owner"
      <*> v .:? "active_form"

taskUpdateDescription :: Text
taskUpdateDescription = "Update a task in the shared task list. Set status, owner, or activeForm."

taskUpdateSchema :: Aeson.Object
taskUpdateSchema = genericToolSchemaWith @TaskUpdateArgs
  [ ("task_id", "The task ID to update"),
    ("status", "The new status (optional)"),
    ("owner", "The new owner (optional)"),
    ("active_form", "The new activeForm (optional)")
  ]

taskUpdateCore :: TaskUpdateArgs -> Eff Effects (Either Text Aeson.Value)
taskUpdateCore args = do
  let req = Tasks.UpdateTaskRequest
        { Tasks.updateTaskRequestTeamName = "",
          Tasks.updateTaskRequestTaskId = TL.fromStrict (tuTaskId args),
          Tasks.updateTaskRequestStatus = maybe "" TL.fromStrict (tuStatus args),
          Tasks.updateTaskRequestOwner = maybe "" TL.fromStrict (tuOwner args),
          Tasks.updateTaskRequestActiveForm = maybe "" TL.fromStrict (tuActiveForm args)
        }
  void $ suspendEffect_ @LogInfo (Log.InfoRequest {Log.infoRequestMessage = "TaskUpdate: Calling update_task effect", Log.infoRequestFields = ""})
  result <- suspendEffect @TasksUpdateTask req
  case result of
    Left err -> pure $ Left (T.pack (show err))
    Right resp -> pure $ Right (Aeson.toJSON resp)

-- MCPTool instances for SDK-level task tools.
-- Roles use these directly (no passthrough wrapper needed).

instance MCPTool TaskList where
  type ToolArgs TaskList = TaskListArgs
  toolName = "task_list"
  toolDescription = taskListDescription
  toolSchema = taskListSchema
  toolHandlerEff args = do
    result <- taskListCore args
    case result of
      Left err -> pure $ errorResult err
      Right output -> pure $ successResult output

instance MCPTool TaskGet where
  type ToolArgs TaskGet = TaskGetArgs
  toolName = "task_get"
  toolDescription = taskGetDescription
  toolSchema = taskGetSchema
  toolHandlerEff args = do
    result <- taskGetCore args
    case result of
      Left err -> pure $ errorResult err
      Right output -> pure $ successResult output

instance MCPTool TaskUpdate where
  type ToolArgs TaskUpdate = TaskUpdateArgs
  toolName = "task_update"
  toolDescription = taskUpdateDescription
  toolSchema = taskUpdateSchema
  toolHandlerEff args = do
    result <- taskUpdateCore args
    case result of
      Left err -> pure $ errorResult err
      Right output -> pure $ successResult output
