{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Prelude hiding (Down)

import Polysemy (Sem, Member, interpret, makeSem)
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

-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT
-- ════════════════════════════════════════════════════════════════════════════

data Habitica m a where
  -- | Original operations (crash on error)
  FetchTodos :: Habitica m [FetchedTodo]
  AddChecklistItem :: TodoId -> Text -> Habitica m ChecklistItemId
  CreateTodo :: Text -> Habitica m TodoId
  GetUser :: Habitica m UserInfo
  ScoreTask :: TaskId -> Direction -> Habitica m ScoreResult
  GetTasks :: TaskType -> Habitica m [HabiticaTask]
  -- | Try variants (return Either)
  FetchTodosTry :: Habitica m (Either HabiticaError [FetchedTodo])
  AddChecklistItemTry :: TodoId -> Text -> Habitica m (Either HabiticaError ChecklistItemId)
  CreateTodoTry :: Text -> Habitica m (Either HabiticaError TodoId)
  GetUserTry :: Habitica m (Either HabiticaError UserInfo)
  ScoreTaskTry :: TaskId -> Direction -> Habitica m (Either HabiticaError ScoreResult)
  GetTasksTry :: TaskType -> Habitica m (Either HabiticaError [HabiticaTask])

makeSem ''Habitica

-- ════════════════════════════════════════════════════════════════════════════
-- STUB RUNNER
-- ════════════════════════════════════════════════════════════════════════════

runHabiticaStub :: (Member Log effs) => Sem (Habitica ': effs) a -> Sem effs a
runHabiticaStub = interpret $ \case
  -- Original operations (crash on call)
  FetchTodos -> do
    logInfo "[Habitica:stub] FetchTodos called"
    error "Habitica.fetchTodos: not implemented"
  AddChecklistItem _ item -> do
    logInfo $ "[Habitica:stub] AddChecklistItem called: " <> item
    error "Habitica.addChecklistItem: not implemented"
  CreateTodo title -> do
    logInfo $ "[Habitica:stub] CreateTodo called: " <> title
    error "Habitica.createTodo: not implemented"
  GetUser -> do
    logInfo "[Habitica:stub] GetUser called"
    error "Habitica.getUser: not implemented"
  ScoreTask tid dir -> do
    logInfo $ "[Habitica:stub] ScoreTask called: " <> tid.unTaskId <> " " <> dirText dir
    error "Habitica.scoreTask: not implemented"
  GetTasks tt -> do
    logInfo $ "[Habitica:stub] GetTasks called: " <> taskTypeText tt
    error "Habitica.getTasks: not implemented"

  -- Try variants (return Left with error instead of crashing)
  FetchTodosTry -> do
    logInfo "[Habitica:stub] FetchTodosTry called"
    pure $ Left (HabiticaOther "Habitica.fetchTodos: not implemented (stub)")
  AddChecklistItemTry _ item -> do
    logInfo $ "[Habitica:stub] AddChecklistItemTry called: " <> item
    pure $ Left (HabiticaOther "Habitica.addChecklistItem: not implemented (stub)")
  CreateTodoTry title -> do
    logInfo $ "[Habitica:stub] CreateTodoTry called: " <> title
    pure $ Left (HabiticaOther "Habitica.createTodo: not implemented (stub)")
  GetUserTry -> do
    logInfo "[Habitica:stub] GetUserTry called"
    pure $ Left (HabiticaOther "Habitica.getUser: not implemented (stub)")
  ScoreTaskTry tid dir -> do
    logInfo $ "[Habitica:stub] ScoreTaskTry called: " <> tid.unTaskId <> " " <> dirText dir
    pure $ Left (HabiticaOther "Habitica.scoreTask: not implemented (stub)")
  GetTasksTry tt -> do
    logInfo $ "[Habitica:stub] GetTasksTry called: " <> taskTypeText tt
    pure $ Left (HabiticaOther "Habitica.getTasks: not implemented (stub)")

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

