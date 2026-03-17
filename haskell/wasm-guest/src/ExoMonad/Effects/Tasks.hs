{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module ExoMonad.Effects.Tasks
  ( TasksListTasks,
    TasksGetTask,
    TasksUpdateTask,
    module Effects.Tasks,
  )
where

import Effects.Tasks
import ExoMonad.Effect.Class (Effect (..))

data TasksListTasks
instance Effect TasksListTasks where
  type Input TasksListTasks = ListTasksRequest
  type Output TasksListTasks = ListTasksResponse
  effectId = "tasks.list_tasks"

data TasksGetTask
instance Effect TasksGetTask where
  type Input TasksGetTask = GetTaskRequest
  type Output TasksGetTask = GetTaskResponse
  effectId = "tasks.get_task"

data TasksUpdateTask
instance Effect TasksUpdateTask where
  type Input TasksUpdateTask = UpdateTaskRequest
  type Output TasksUpdateTask = UpdateTaskResponse
  effectId = "tasks.update_task"