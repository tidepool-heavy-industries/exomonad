{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | Habitica integration effect for Polysemy.
--
-- This module provides the Polysemy effect interface for Habitica operations.
-- Types are imported from "ExoMonad.Habitica".
module ExoMonad.Effects.Habitica
  ( -- * Effect
    Habitica (..),
    fetchTodos,
    addChecklistItem,
    createTodo,
    getUser,
    scoreTask,
    getTasks,

    -- * Try Variants (return Either instead of crashing)
    fetchTodosTry,
    addChecklistItemTry,
    createTodoTry,
    getUserTry,
    scoreTaskTry,
    getTasksTry,

    -- * Re-exported Types
    TaskType (..),
    Direction (..),
    TaskId (..),
    TodoId (..),
    ChecklistItemId (..),
    UserInfo (..),
    UserStats (..),
    HabiticaTask (..),
    FetchedTodo (..),
    FetchedChecklistItem (..),
    ScoreResult (..),
    HabiticaError (..),

    -- * Runner (stub)
    runHabiticaStub,
  )
where

import ExoMonad.Effect (Log, logInfo)
import ExoMonad.Habitica
  ( ChecklistItemId (..),
    Direction (..),
    FetchedChecklistItem (..),
    FetchedTodo (..),
    HabiticaError (..),
    HabiticaTask (..),
    ScoreResult (..),
    TaskId (..),
    TaskType (..),
    TodoId (..),
    UserInfo (..),
    UserStats (..),
  )
import Polysemy (Member, Sem, interpret, makeSem)
import Polysemy.Error (Error, throw)
import Prelude hiding (Down)

-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT
-- ════════════════════════════════════════════════════════════════════════════

data Habitica m a where
  FetchTodos :: Habitica m [FetchedTodo]
  AddChecklistItem :: TodoId -> Text -> Habitica m ChecklistItemId
  CreateTodo :: Text -> Habitica m TodoId
  GetUser :: Habitica m UserInfo
  ScoreTask :: TaskId -> Direction -> Habitica m ScoreResult
  GetTasks :: TaskType -> Habitica m [HabiticaTask]

makeSem ''Habitica

-- ════════════════════════════════════════════════════════════════════════════
-- STUB RUNNER
-- ════════════════════════════════════════════════════════════════════════════

runHabiticaStub :: (Member Log effs, Member (Error HabiticaError) effs) => Sem (Habitica ': effs) a -> Sem effs a
runHabiticaStub = interpret $ \case
  FetchTodos -> do
    logInfo "[Habitica:stub] FetchTodos called"
    throw $ HabiticaOther "Stub: not implemented"
  AddChecklistItem _ item -> do
    logInfo $ "[Habitica:stub] AddChecklistItem called: " <> item
    throw $ HabiticaOther "Stub: not implemented"
  CreateTodo title -> do
    logInfo $ "[Habitica:stub] CreateTodo called: " <> title
    throw $ HabiticaOther "Stub: not implemented"
  GetUser -> do
    logInfo "[Habitica:stub] GetUser called"
    throw $ HabiticaOther "Stub: not implemented"
  ScoreTask tid dir -> do
    logInfo $ "[Habitica:stub] ScoreTask called: " <> tid.unTaskId <> " " <> dirText dir
    throw $ HabiticaOther "Stub: not implemented"
  GetTasks tt -> do
    logInfo $ "[Habitica:stub] GetTasks called: " <> taskTypeText tt
    throw $ HabiticaOther "Stub: not implemented"

-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

dirText :: Direction -> Text
dirText Up = "Up"
dirText Down = "Down"

taskTypeText :: TaskType -> Text
taskTypeText Habits = "Habits"
taskTypeText Dailys = "Dailys"
taskTypeText Todos = "Todos"
taskTypeText Rewards = "Rewards"
