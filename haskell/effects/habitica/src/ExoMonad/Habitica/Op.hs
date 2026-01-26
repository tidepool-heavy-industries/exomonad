-- | Type-safe Habitica operations.
--
-- This module provides a GADT where each operation specifies its exact
-- payload and return type:
--
-- @
-- GetUser          -- Returns UserInfo
-- GetTasks Dailys  -- Returns [HabiticaTask]
-- CreateTodo title -- Returns TodoId
-- @
--
-- If it compiles, the payload shape is correct.
module ExoMonad.Habitica.Op
  ( HabiticaOp(..)
  ) where

import Data.Text (Text)

import ExoMonad.Habitica.Types
  ( TaskId
  , TodoId
  , ChecklistItemId
  , TaskType
  , Direction
  )
import ExoMonad.Habitica.Response
  ( UserInfo
  , HabiticaTask
  , FetchedTodo
  , ScoreResult
  )


-- | Type-safe Habitica operations.
--
-- Each constructor specifies exactly what payload it needs and what it returns.
data HabiticaOp a where
  -- | Get current user info and stats.
  GetUser :: HabiticaOp UserInfo

  -- | Get tasks of a specific type.
  GetTasks :: TaskType -> HabiticaOp [HabiticaTask]

  -- | Fetch all todos with their checklists.
  FetchTodos :: HabiticaOp [FetchedTodo]

  -- | Score a task (mark habit/daily done, etc).
  ScoreTask :: TaskId -> Direction -> HabiticaOp ScoreResult

  -- | Create a new todo. Returns the new todo's ID.
  CreateTodo :: Text -> HabiticaOp TodoId

  -- | Add a checklist item to a todo. Returns the new item's ID.
  AddChecklistItem :: TodoId -> Text -> HabiticaOp ChecklistItemId
