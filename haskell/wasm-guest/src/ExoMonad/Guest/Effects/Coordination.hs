{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

-- | Low-level coordination effects for tasks and messaging.
module ExoMonad.Guest.Effects.Coordination
  ( -- * Effect Types
    CoordinationCreateTask,
    CoordinationUpdateTask,
    CoordinationListTasks,
    CoordinationGetTask,
    CoordinationSendMessage,
    CoordinationGetMessages,

    -- * Smart Constructors
    createTask,
    updateTask,
    listTasks,
    getTask,
    sendMessage,
    getMessages,

    -- * Re-exported proto types
    module Effects.Coordination,
    Enumerated (..),
  )
where

import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Vector qualified as V
import Effects.Coordination
import Effects.EffectError (EffectError)
import ExoMonad.Effect.Class (Effect (..), runEffect)
import Proto3.Suite.Types (Enumerated (..))

-- ============================================================================
-- Effect phantom types + instances
-- ============================================================================

data CoordinationCreateTask

instance Effect CoordinationCreateTask where
  type Input CoordinationCreateTask = CreateTaskRequest
  type Output CoordinationCreateTask = CreateTaskResponse
  effectId = "coordination.create_task"

data CoordinationUpdateTask

instance Effect CoordinationUpdateTask where
  type Input CoordinationUpdateTask = UpdateTaskRequest
  type Output CoordinationUpdateTask = UpdateTaskResponse
  effectId = "coordination.update_task"

data CoordinationListTasks

instance Effect CoordinationListTasks where
  type Input CoordinationListTasks = ListTasksRequest
  type Output CoordinationListTasks = ListTasksResponse
  effectId = "coordination.list_tasks"

data CoordinationGetTask

instance Effect CoordinationGetTask where
  type Input CoordinationGetTask = GetTaskRequest
  type Output CoordinationGetTask = GetTaskResponse
  effectId = "coordination.get_task"

data CoordinationSendMessage

instance Effect CoordinationSendMessage where
  type Input CoordinationSendMessage = SendMessageRequest
  type Output CoordinationSendMessage = SendMessageResponse
  effectId = "coordination.send_message"

data CoordinationGetMessages

instance Effect CoordinationGetMessages where
  type Input CoordinationGetMessages = GetMessagesRequest
  type Output CoordinationGetMessages = GetMessagesResponse
  effectId = "coordination.get_messages"

-- ============================================================================
-- Smart constructors
-- ============================================================================

-- | Create a new task.
createTask ::
  Text -> -- ^ Subject
  Text -> -- ^ Description
  Text -> -- ^ Owner
  [Text] -> -- ^ Blocked by
  IO (Either EffectError CreateTaskResponse)
createTask subject description owner blockedBy =
  runEffect @CoordinationCreateTask $
    CreateTaskRequest
      { createTaskRequestSubject = TL.fromStrict subject,
        createTaskRequestDescription = TL.fromStrict description,
        createTaskRequestOwner = TL.fromStrict owner,
        createTaskRequestBlockedBy = V.fromList (map TL.fromStrict blockedBy)
      }

-- | Update an existing task.
updateTask ::
  Text -> -- ^ Task ID
  TaskStatus -> -- ^ New status
  Text -> -- ^ New owner
  Text -> -- ^ New description
  Text -> -- ^ New subject
  IO (Either EffectError UpdateTaskResponse)
updateTask taskId status owner description subject =
  runEffect @CoordinationUpdateTask $
    UpdateTaskRequest
      { updateTaskRequestTaskId = TL.fromStrict taskId,
        updateTaskRequestStatus = Enumerated (Right status),
        updateTaskRequestOwner = TL.fromStrict owner,
        updateTaskRequestDescription = TL.fromStrict description,
        updateTaskRequestSubject = TL.fromStrict subject
      }

-- | List tasks with an optional status filter.
listTasks ::
  TaskStatus -> -- ^ Filter status
  IO (Either EffectError ListTasksResponse)
listTasks status =
  runEffect @CoordinationListTasks $
    ListTasksRequest
      { listTasksRequestFilterStatus = Enumerated (Right status)
      }

-- | Get a single task by ID.
getTask ::
  Text -> -- ^ Task ID
  IO (Either EffectError GetTaskResponse)
getTask taskId =
  runEffect @CoordinationGetTask $
    GetTaskRequest
      { getTaskRequestTaskId = TL.fromStrict taskId
      }

-- | Send a coordination message.
sendMessage ::
  Text -> -- ^ From
  Text -> -- ^ Text
  Text -> -- ^ Summary
  IO (Either EffectError SendMessageResponse)
sendMessage from msg summary =
  runEffect @CoordinationSendMessage $
    SendMessageRequest
      { sendMessageRequestFrom = TL.fromStrict from,
        sendMessageRequestText = TL.fromStrict msg,
        sendMessageRequestSummary = TL.fromStrict summary
      }

-- | Get coordination messages.
getMessages ::
  Bool -> -- ^ Unread only
  IO (Either EffectError GetMessagesResponse)
getMessages unreadOnly =
  runEffect @CoordinationGetMessages $
    GetMessagesRequest
      { getMessagesRequestUnreadOnly = unreadOnly
      }
