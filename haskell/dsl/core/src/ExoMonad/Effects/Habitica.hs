-- | Habitica integration effect for freer-simple.
--
-- This module provides the freer-simple effect interface for Habitica operations.
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

import Control.Monad.Freer (Eff, Member, interpret, send)
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

data Habitica r where
  -- | Original operations (crash on error)
  FetchTodos :: Habitica [FetchedTodo]
  AddChecklistItem :: TodoId -> Text -> Habitica ChecklistItemId
  CreateTodo :: Text -> Habitica TodoId
  GetUser :: Habitica UserInfo
  ScoreTask :: TaskId -> Direction -> Habitica ScoreResult
  GetTasks :: TaskType -> Habitica [HabiticaTask]
  -- | Try variants (return Either)
  FetchTodosTry :: Habitica (Either HabiticaError [FetchedTodo])
  AddChecklistItemTry :: TodoId -> Text -> Habitica (Either HabiticaError ChecklistItemId)
  CreateTodoTry :: Text -> Habitica (Either HabiticaError TodoId)
  GetUserTry :: Habitica (Either HabiticaError UserInfo)
  ScoreTaskTry :: TaskId -> Direction -> Habitica (Either HabiticaError ScoreResult)
  GetTasksTry :: TaskType -> Habitica (Either HabiticaError [HabiticaTask])

-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

fetchTodos :: (Member Habitica effs) => Eff effs [FetchedTodo]
fetchTodos = send FetchTodos

addChecklistItem :: (Member Habitica effs) => TodoId -> Text -> Eff effs ChecklistItemId
addChecklistItem tid item = send (AddChecklistItem tid item)

createTodo :: (Member Habitica effs) => Text -> Eff effs TodoId
createTodo title = send (CreateTodo title)

getUser :: (Member Habitica effs) => Eff effs UserInfo
getUser = send GetUser

scoreTask :: (Member Habitica effs) => TaskId -> Direction -> Eff effs ScoreResult
scoreTask tid dir = send (ScoreTask tid dir)

getTasks :: (Member Habitica effs) => TaskType -> Eff effs [HabiticaTask]
getTasks tt = send (GetTasks tt)

-- ════════════════════════════════════════════════════════════════════════════
-- TRY VARIANTS
-- ════════════════════════════════════════════════════════════════════════════

fetchTodosTry :: (Member Habitica effs) => Eff effs (Either HabiticaError [FetchedTodo])
fetchTodosTry = send FetchTodosTry

addChecklistItemTry :: (Member Habitica effs) => TodoId -> Text -> Eff effs (Either HabiticaError ChecklistItemId)
addChecklistItemTry tid item = send (AddChecklistItemTry tid item)

createTodoTry :: (Member Habitica effs) => Text -> Eff effs (Either HabiticaError TodoId)
createTodoTry title = send (CreateTodoTry title)

getUserTry :: (Member Habitica effs) => Eff effs (Either HabiticaError UserInfo)
getUserTry = send GetUserTry

scoreTaskTry :: (Member Habitica effs) => TaskId -> Direction -> Eff effs (Either HabiticaError ScoreResult)
scoreTaskTry tid dir = send (ScoreTaskTry tid dir)

getTasksTry :: (Member Habitica effs) => TaskType -> Eff effs (Either HabiticaError [HabiticaTask])
getTasksTry tt = send (GetTasksTry tt)

-- ════════════════════════════════════════════════════════════════════════════
-- STUB RUNNER
-- ════════════════════════════════════════════════════════════════════════════

runHabiticaStub :: (Member Log effs) => Eff (Habitica ': effs) a -> Eff effs a
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
