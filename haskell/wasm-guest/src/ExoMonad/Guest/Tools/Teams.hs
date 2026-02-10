{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Teams task management tool definitions for Gemini agents.
--
-- Six tools that let Gemini agents participate in Claude Code Teams protocol
-- by reading/writing the same filesystem structures that Claude's native
-- TaskCreate/TaskUpdate/TaskList tools use.
module ExoMonad.Guest.Tools.Teams
  ( -- * Tool types
    ClaimTask,
    CompleteTask,
    ListTasks,
    GetTask,
    ReportStatus,
    AskQuestion,

    -- * Argument types (exported for tests)
    ClaimTaskArgs (..),
    CompleteTaskArgs (..),
    ListTasksArgs (..),
    GetTaskArgs (..),
    ReportStatusArgs (..),
    AskQuestionArgs (..),
  )
where

import Data.Aeson (FromJSON, Value, object, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Effects.Teams qualified as T
import ExoMonad.Effects.Teams qualified as Teams
import ExoMonad.Guest.Tool.Class
import GHC.Generics (Generic)
import Proto3.Suite.Types (Enumerated (..))

-- ============================================================================
-- ClaimTask
-- ============================================================================

-- | Self-assign a pending, unblocked task.
data ClaimTask

newtype ClaimTaskArgs = ClaimTaskArgs
  { ctTaskId :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON ClaimTaskArgs where
  parseJSON = Aeson.withObject "ClaimTaskArgs" $ \v ->
    ClaimTaskArgs
      <$> v .: "task_id"

instance MCPTool ClaimTask where
  type ToolArgs ClaimTask = ClaimTaskArgs
  toolName = "claim_task"
  toolDescription = "Self-assign a pending, unblocked task from the shared task list. Call list_tasks first to see available work. Only tasks with status 'pending' and empty blockedBy can be claimed. After claiming, read the task description carefully and begin implementation immediately. Prefer claiming tasks in ID order (lowest first) as earlier tasks often set up context for later ones."
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "required" .= (["task_id"] :: [Text]),
        "properties"
          .= object
            [ "task_id"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("ID of the task to claim. Get this from list_tasks output." :: Text)
                  ]
            ]
      ]
  toolHandler args = do
    result <- Teams.claimTask (ctTaskId args)
    case result of
      Left err -> pure $ errorResult (TL.toStrict $ "Teams effect failed: " <> TL.pack (show err))
      Right resp
        | T.claimTaskResponseSuccess resp ->
            pure $ successResult $ Aeson.toJSON (taskInfoToValue <$> T.claimTaskResponseTask resp)
        | otherwise ->
            pure $ errorResult (TL.toStrict $ T.claimTaskResponseError resp)

-- ============================================================================
-- CompleteTask
-- ============================================================================

-- | Mark a task as completed with a summary.
data CompleteTask

data CompleteTaskArgs = CompleteTaskArgs
  { coTaskId :: Text,
    coSummary :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON CompleteTaskArgs where
  parseJSON = Aeson.withObject "CompleteTaskArgs" $ \v ->
    CompleteTaskArgs
      <$> v .: "task_id"
      <*> v .: "summary"

instance MCPTool CompleteTask where
  type ToolArgs CompleteTask = CompleteTaskArgs
  toolName = "complete_task"
  toolDescription = "Mark a task as completed with a summary of what was accomplished. You must be the owner of the task (i.e., you claimed it). After completing, call list_tasks to find your next available task. Include enough detail in the summary that the team lead can understand what changed without reading the code."
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "required" .= (["task_id", "summary"] :: [Text]),
        "properties"
          .= object
            [ "task_id"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("ID of the task you are completing. Must be a task you previously claimed." :: Text)
                  ],
              "summary"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Summary of what was accomplished. Be specific: which files changed, what was implemented, what was tested. The team lead uses this to track progress." :: Text)
                  ]
            ]
      ]
  toolHandler args = do
    result <- Teams.completeTask (coTaskId args) (coSummary args)
    case result of
      Left err -> pure $ errorResult (TL.toStrict $ "Teams effect failed: " <> TL.pack (show err))
      Right resp
        | T.completeTaskResponseSuccess resp ->
            pure $ successResult $ Aeson.toJSON (taskInfoToValue <$> T.completeTaskResponseTask resp)
        | otherwise ->
            pure $ errorResult (TL.toStrict $ T.completeTaskResponseError resp)

-- ============================================================================
-- ListTasks
-- ============================================================================

-- | List all tasks with status, owner, and dependencies.
data ListTasks

data ListTasksArgs = ListTasksArgs
  deriving (Show, Eq, Generic)

instance FromJSON ListTasksArgs where
  parseJSON = Aeson.withObject "ListTasksArgs" $ \_ ->
    pure ListTasksArgs

instance MCPTool ListTasks where
  type ToolArgs ListTasks = ListTasksArgs
  toolName = "list_tasks"
  toolDescription = "List all tasks in the team's shared task list. Returns every task with its status, owner, and dependency info. Use this to find available work (status 'pending', no owner, empty blockedBy), check what teammates are working on, and understand the overall project state. Call this when you start a session and after completing each task."
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "properties" .= object mempty
      ]
  toolHandler _args = do
    result <- Teams.listTasks
    case result of
      Left err -> pure $ errorResult (TL.toStrict $ "Teams effect failed: " <> TL.pack (show err))
      Right resp ->
        let tasks = map taskInfoToValue (toList $ T.listTasksResponseTasks resp)
         in pure $ successResult $ Aeson.toJSON tasks
    where
      toList = foldr (:) []

-- ============================================================================
-- GetTask
-- ============================================================================

-- | Read a single task's full details.
data GetTask

newtype GetTaskArgs = GetTaskArgs
  { gtTaskId :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON GetTaskArgs where
  parseJSON = Aeson.withObject "GetTaskArgs" $ \v ->
    GetTaskArgs
      <$> v .: "task_id"

instance MCPTool GetTask where
  type ToolArgs GetTask = GetTaskArgs
  toolName = "get_task"
  toolDescription = "Read a single task's full details including the complete description, acceptance criteria, and dependencies. Use this after list_tasks to get the full context before starting work on a task. The description contains everything you need to know about what to implement."
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "required" .= (["task_id"] :: [Text]),
        "properties"
          .= object
            [ "task_id"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("ID of the task to read. Get this from list_tasks output." :: Text)
                  ]
            ]
      ]
  toolHandler args = do
    result <- Teams.getTask (gtTaskId args)
    case result of
      Left err -> pure $ errorResult (TL.toStrict $ "Teams effect failed: " <> TL.pack (show err))
      Right resp ->
        case T.getTaskResponseTask resp of
          Nothing -> pure $ errorResult "Task not found"
          Just task -> pure $ successResult $ taskInfoToValue task

-- ============================================================================
-- ReportStatus
-- ============================================================================

-- | Send a non-blocking status update to the TL.
data ReportStatus

newtype ReportStatusArgs = ReportStatusArgs
  { rsContent :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON ReportStatusArgs where
  parseJSON = Aeson.withObject "ReportStatusArgs" $ \v ->
    ReportStatusArgs
      <$> v .: "content"

instance MCPTool ReportStatus where
  type ToolArgs ReportStatus = ReportStatusArgs
  toolName = "report_status"
  toolDescription = "Send a non-blocking status update to the team lead. Fire-and-forget — does NOT wait for a response. Use for progress updates, noting blockers, sharing findings, or flagging risks. Prefer this over ask_question unless you need an answer to continue working. Good practice: report status after significant milestones or when encountering unexpected issues."
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "required" .= (["content"] :: [Text]),
        "properties"
          .= object
            [ "content"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Status update message. Include what you've done, what you're doing next, and any blockers or concerns. Be concise but specific." :: Text)
                  ]
            ]
      ]
  toolHandler args = do
    result <- Teams.reportStatus (rsContent args)
    case result of
      Left err -> pure $ errorResult (TL.toStrict $ "Teams effect failed: " <> TL.pack (show err))
      Right _resp -> pure $ successResult $ object ["ack" .= True]

-- ============================================================================
-- AskQuestion
-- ============================================================================

-- | Send a blocking question to the TL and wait for an answer.
data AskQuestion

newtype AskQuestionArgs = AskQuestionArgs
  { aqQuestion :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON AskQuestionArgs where
  parseJSON = Aeson.withObject "AskQuestionArgs" $ \v ->
    AskQuestionArgs
      <$> v .: "question"

instance MCPTool AskQuestion where
  type ToolArgs AskQuestion = AskQuestionArgs
  toolName = "ask_question"
  toolDescription = "Ask the team lead a question and wait for their response (up to 5 minutes). Good reasons to ask: unclear requirements, choosing between approaches, unsure about scope, need context about the codebase, or want a sanity check before a big change. Asking early saves time — it's always better to ask than to guess wrong. Note: this blocks until answered, so for fire-and-forget updates use report_status instead."
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "required" .= (["question"] :: [Text]),
        "properties"
          .= object
            [ "question"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Your question. Include relevant context so the team lead can help quickly — what you're working on, what you've tried or considered, and what you'd recommend if you have a preference." :: Text)
                  ]
            ]
      ]
  toolHandler args = do
    result <- Teams.askQuestion (aqQuestion args)
    case result of
      Left err -> pure $ errorResult (TL.toStrict $ "Teams effect failed: " <> TL.pack (show err))
      Right resp -> pure $ successResult $ object ["answer" .= TL.toStrict (T.askQuestionResponseAnswer resp)]

-- ============================================================================
-- Helpers
-- ============================================================================

-- | Convert a proto TaskInfo to a JSON Value for MCP response.
taskInfoToValue :: T.TaskInfo -> Value
taskInfoToValue t =
  object
    [ "id" .= TL.toStrict (T.taskInfoId t),
      "subject" .= TL.toStrict (T.taskInfoSubject t),
      "description" .= TL.toStrict (T.taskInfoDescription t),
      "activeForm" .= TL.toStrict (T.taskInfoActiveForm t),
      "status" .= statusToText (T.taskInfoStatus t),
      "blocks" .= map TL.toStrict (toList $ T.taskInfoBlocks t),
      "blockedBy" .= map TL.toStrict (toList $ T.taskInfoBlockedBy t),
      "owner" .= TL.toStrict (T.taskInfoOwner t)
    ]
  where
    toList = foldr (:) []
    statusToText (Enumerated (Right T.TaskStatusTASK_STATUS_PENDING)) = "pending" :: Text
    statusToText (Enumerated (Right T.TaskStatusTASK_STATUS_IN_PROGRESS)) = "in_progress"
    statusToText (Enumerated (Right T.TaskStatusTASK_STATUS_COMPLETED)) = "completed"
    statusToText _ = "unknown"
