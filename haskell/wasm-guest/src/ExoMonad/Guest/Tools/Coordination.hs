{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Coordination tool definitions and handlers.
module ExoMonad.Guest.Tools.Coordination
  ( -- * Tool types
    CreateTask,
    UpdateTask,
    ListTasks,
    GetTask,
    SendCoordMessage,
    GetCoordMessages,

    -- * Argument types (exported for tests)
    CreateTaskArgs (..),
    UpdateTaskArgs (..),
    ListTasksArgs (..),
    GetTaskArgs (..),
    SendCoordMessageArgs (..),
    GetCoordMessagesArgs (..),
  )
where

import Data.Aeson (FromJSON, Value, object, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Vector qualified as V
import Effects.Kv qualified as KV
import ExoMonad.Effects.KV (kvGet)
import ExoMonad.Guest.Effects.Coordination qualified as Coordination
import ExoMonad.Guest.Tool.Class
import GHC.Generics (Generic)

-- ============================================================================
-- CreateTask
-- ============================================================================

data CreateTask

data CreateTaskArgs = CreateTaskArgs
  { ctSubject :: Text,
    ctDescription :: Maybe Text,
    ctOwner :: Maybe Text,
    ctBlockedBy :: Maybe [Text]
  }
  deriving (Show, Eq, Generic)

instance FromJSON CreateTaskArgs where
  parseJSON = Aeson.withObject "CreateTaskArgs" $ \v ->
    CreateTaskArgs
      <$> v .: "subject"
      <*> v .:? "description"
      <*> v .:? "owner"
      <*> v .:? "blocked_by"

instance MCPTool CreateTask where
  type ToolArgs CreateTask = CreateTaskArgs
  toolName = "create_task"
  toolDescription = "Create a new task in the coordination system"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "required" .= (["subject"] :: [Text]),
        "properties"
          .= object
            [ "subject"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Short subject of the task" :: Text)
                  ],
              "description"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Detailed description of the task" :: Text)
                  ],
              "owner"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Owner of the task (agent ID or team name)" :: Text)
                  ],
              "blocked_by"
                .= object
                  [ "type" .= ("array" :: Text),
                    "items" .= object ["type" .= ("string" :: Text)],
                    "description" .= ("List of task IDs that block this task" :: Text)
                  ]
            ]
      ]
  toolHandler args = do
    let subject = ctSubject args
        description = fromMaybe "" (ctDescription args)
        owner = fromMaybe "" (ctOwner args)
        blockedBy = fromMaybe [] (ctBlockedBy args)
    result <- Coordination.createTask subject description owner blockedBy
    case result of
      Left err -> pure $ errorResult (TL.toStrict $ "Coordination effect failed: " <> TL.pack (show err))
      Right resp -> pure $ successResult $ object ["task_id" .= TL.toStrict (Coordination.createTaskResponseTaskId resp)]

-- ============================================================================
-- UpdateTask
-- ============================================================================

data UpdateTask

data UpdateTaskArgs = UpdateTaskArgs
  { utTaskId :: Text,
    utStatus :: Maybe Text,
    utOwner :: Maybe Text,
    utDescription :: Maybe Text,
    utSubject :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON UpdateTaskArgs where
  parseJSON = Aeson.withObject "UpdateTaskArgs" $ \v ->
    UpdateTaskArgs
      <$> v .: "task_id"
      <*> v .:? "status"
      <*> v .:? "owner"
      <*> v .:? "description"
      <*> v .:? "subject"

instance MCPTool UpdateTask where
  type ToolArgs UpdateTask = UpdateTaskArgs
  toolName = "update_task"
  toolDescription = "Update an existing task's status, owner, or details"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "required" .= (["task_id"] :: [Text]),
        "properties"
          .= object
            [ "task_id"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("ID of the task to update" :: Text)
                  ],
              "status"
                .= object
                  [ "type" .= ("string" :: Text),
                    "enum" .= (["pending", "in_progress", "completed"] :: [Text]),
                    "description" .= ("New status of the task" :: Text)
                  ],
              "owner"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("New owner of the task" :: Text)
                  ],
              "description"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("New description of the task" :: Text)
                  ],
              "subject"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("New subject of the task" :: Text)
                  ]
            ]
      ]
  toolHandler args = do
    -- We need current values for fields not being updated.
    -- The simplest way is to fetch the task first, but Coordination.getTask
    -- returns a Task if found.
    getRes <- Coordination.getTask (utTaskId args)
    case getRes of
      Left err -> pure $ errorResult (TL.toStrict $ "Coordination effect failed (get): " <> TL.pack (show err))
      Right getResp -> case Coordination.getTaskResponseTask getResp of
        Nothing -> pure $ errorResult $ "Task not found: " <> utTaskId args
        Just task -> do
          let status = maybe (Coordination.taskStatus task) parseStatus (utStatus args)
              owner = fromMaybe (TL.toStrict $ Coordination.taskOwner task) (utOwner args)
              description = fromMaybe (TL.toStrict $ Coordination.taskDescription task) (utDescription args)
              subject = fromMaybe (TL.toStrict $ Coordination.taskSubject task) (utSubject args)
          
          res <- Coordination.updateTask (utTaskId args) (unwrapEnum status) owner description subject
          case res of
            Left err -> pure $ errorResult (TL.toStrict $ "Coordination effect failed (update): " <> TL.pack (show err))
            Right resp -> pure $ successResult $ object ["success" .= Coordination.updateTaskResponseSuccess resp]

parseStatus :: Text -> Coordination.Enumerated Coordination.TaskStatus
parseStatus "pending" = Coordination.Enumerated (Right Coordination.TaskStatusTASK_STATUS_PENDING)
parseStatus "in_progress" = Coordination.Enumerated (Right Coordination.TaskStatusTASK_STATUS_IN_PROGRESS)
parseStatus "completed" = Coordination.Enumerated (Right Coordination.TaskStatusTASK_STATUS_COMPLETED)
parseStatus _ = Coordination.Enumerated (Right Coordination.TaskStatusTASK_STATUS_UNSPECIFIED)

unwrapEnum :: Coordination.Enumerated Coordination.TaskStatus -> Coordination.TaskStatus
unwrapEnum (Coordination.Enumerated (Right s)) = s
unwrapEnum _ = Coordination.TaskStatusTASK_STATUS_UNSPECIFIED

-- ============================================================================
-- ListTasks
-- ============================================================================

data ListTasks

data ListTasksArgs = ListTasksArgs
  { ltStatus :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON ListTasksArgs where
  parseJSON = Aeson.withObject "ListTasksArgs" $ \v ->
    ListTasksArgs <$> v .:? "status"

instance MCPTool ListTasks where
  type ToolArgs ListTasks = ListTasksArgs
  toolName = "list_tasks"
  toolDescription = "List tasks, optionally filtered by status"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "status"
                .= object
                  [ "type" .= ("string" :: Text),
                    "enum" .= (["pending", "in_progress", "completed"] :: [Text]),
                    "description" .= ("Filter tasks by status" :: Text)
                  ]
            ]
      ]
  toolHandler args = do
    let status = maybe Coordination.TaskStatusTASK_STATUS_UNSPECIFIED (unwrapEnum . parseStatus) (ltStatus args)
    result <- Coordination.listTasks status
    case result of
      Left err -> pure $ errorResult (TL.toStrict $ "Coordination effect failed: " <> TL.pack (show err))
      Right resp ->
        let tasks = V.toList (Coordination.listTasksResponseTasks resp)
         in pure $ successResult $ Aeson.toJSON (map taskToJson tasks)

taskToJson :: Coordination.Task -> Value
taskToJson t =
  object
    [ "id" .= TL.toStrict (Coordination.taskId t),
      "subject" .= TL.toStrict (Coordination.taskSubject t),
      "description" .= TL.toStrict (Coordination.taskDescription t),
      "status" .= statusToText (Coordination.taskStatus t),
      "owner" .= TL.toStrict (Coordination.taskOwner t),
      "blocked_by" .= map TL.toStrict (V.toList (Coordination.taskBlockedBy t))
    ]

statusToText :: Coordination.Enumerated Coordination.TaskStatus -> Text
statusToText (Coordination.Enumerated (Right Coordination.TaskStatusTASK_STATUS_PENDING)) = "pending"
statusToText (Coordination.Enumerated (Right Coordination.TaskStatusTASK_STATUS_IN_PROGRESS)) = "in_progress"
statusToText (Coordination.Enumerated (Right Coordination.TaskStatusTASK_STATUS_COMPLETED)) = "completed"
statusToText _ = "unspecified"

-- ============================================================================
-- GetTask
-- ============================================================================

data GetTask

data GetTaskArgs = GetTaskArgs
  { gtTaskId :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON GetTaskArgs where
  parseJSON = Aeson.withObject "GetTaskArgs" $ \v ->
    GetTaskArgs <$> v .: "task_id"

instance MCPTool GetTask where
  type ToolArgs GetTask = GetTaskArgs
  toolName = "get_task"
  toolDescription = "Get details of a single task by ID"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "required" .= (["task_id"] :: [Text]),
        "properties"
          .= object
            [ "task_id"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("The task ID to fetch" :: Text)
                  ]
            ]
      ]
  toolHandler args = do
    result <- Coordination.getTask (gtTaskId args)
    case result of
      Left err -> pure $ errorResult (TL.toStrict $ "Coordination effect failed: " <> TL.pack (show err))
      Right resp -> case Coordination.getTaskResponseTask resp of
        Nothing -> pure $ errorResult $ "Task not found: " <> gtTaskId args
        Just task -> pure $ successResult $ taskToJson task

-- ============================================================================
-- SendCoordMessage
-- ============================================================================

data SendCoordMessage

data SendCoordMessageArgs = SendCoordMessageArgs
  { smText :: Text,
    smSummary :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON SendCoordMessageArgs where
  parseJSON = Aeson.withObject "SendCoordMessageArgs" $ \v ->
    SendCoordMessageArgs
      <$> v .: "text"
      <*> v .:? "summary"

instance MCPTool SendCoordMessage where
  type ToolArgs SendCoordMessage = SendCoordMessageArgs
  toolName = "coord_send_message"
  toolDescription = "Send a message to the coordination system"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "required" .= (["text"] :: [Text]),
        "properties"
          .= object
            [ "text"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Message text" :: Text)
                  ],
              "summary"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Brief summary of the message" :: Text)
                  ]
            ]
      ]
  toolHandler args = do
    teamName <- resolveTeamName
    let from = fromMaybe "agent" teamName
        summary = fromMaybe "" (smSummary args)
    result <- Coordination.sendMessage from (smText args) summary
    case result of
      Left err -> pure $ errorResult (TL.toStrict $ "Coordination effect failed: " <> TL.pack (show err))
      Right resp -> pure $ successResult $ object ["success" .= Coordination.sendMessageResponseSuccess resp]

-- ============================================================================
-- GetCoordMessages
-- ============================================================================

data GetCoordMessages

data GetCoordMessagesArgs = GetCoordMessagesArgs
  { gmUnreadOnly :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON GetCoordMessagesArgs where
  parseJSON = Aeson.withObject "GetCoordMessagesArgs" $ \v ->
    GetCoordMessagesArgs <$> v .:? "unread_only"

instance MCPTool GetCoordMessages where
  type ToolArgs GetCoordMessages = GetCoordMessagesArgs
  toolName = "coord_get_messages"
  toolDescription = "Get messages from the coordination system"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "unread_only"
                .= object
                  [ "type" .= ("boolean" :: Text),
                    "description" .= ("If true, only fetch unread messages" :: Text)
                  ]
            ]
      ]
  toolHandler args = do
    let unreadOnly = fromMaybe False (gmUnreadOnly args)
    result <- Coordination.getMessages unreadOnly
    case result of
      Left err -> pure $ errorResult (TL.toStrict $ "Coordination effect failed: " <> TL.pack (show err))
      Right resp ->
        let messages = V.toList (Coordination.getMessagesResponseMessages resp)
         in pure $ successResult $ Aeson.toJSON (map messageToJson messages)

messageToJson :: Coordination.AgentMessage -> Value
messageToJson m =
  object
    [ "from" .= TL.toStrict (Coordination.agentMessageFrom m),
      "text" .= TL.toStrict (Coordination.agentMessageText m),
      "summary" .= TL.toStrict (Coordination.agentMessageSummary m),
      "timestamp" .= TL.toStrict (Coordination.agentMessageTimestamp m),
      "read" .= Coordination.agentMessageRead m
    ]

-- ============================================================================
-- Helpers
-- ============================================================================

-- | Resolve team name from KV store (set by PostToolUse hook on TeamCreate).
resolveTeamName :: IO (Maybe Text)
resolveTeamName = do
  resp <- kvGet KV.GetRequest {KV.getRequestKey = "current_team"}
  pure $ case resp of
    Right r | KV.getResponseFound r -> Just (TL.toStrict (KV.getResponseValue r))
    _ -> Nothing
