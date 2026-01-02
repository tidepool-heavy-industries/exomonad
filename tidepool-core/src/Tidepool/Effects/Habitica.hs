-- | Habitica integration effect
module Tidepool.Effects.Habitica
  ( -- * Effect
    Habitica(..)
  , fetchTodos
  , addChecklistItem
  , createTodo
  , getUser
  , scoreTask
  , getTasks

    -- * Types
  , Todo(..)
  , TodoId(..)
  , ChecklistItem(..)
  , TaskId(..)
  , Direction(..)
  , TaskType(..)
  , User(..)
  , UserStats(..)
  , Task(..)
  , ScoreResult(..)

    -- * Runner (stub)
  , runHabiticaStub
  ) where

import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Control.Monad.Freer (Eff, Member, send, interpret)

import Tidepool.Effect (Log, logInfo)

-- Types

newtype TodoId = TodoId { unTodoId :: Text }
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

data Todo = Todo
  { todoId        :: TodoId
  , todoTitle     :: Text
  , todoChecklist :: [ChecklistItem]
  , todoCompleted :: Bool
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data ChecklistItem = ChecklistItem
  { checklistId   :: Text
  , checklistText :: Text
  , checklistDone :: Bool
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype TaskId = TaskId { unTaskId :: Text }
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

data Direction = Up | Down
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data TaskType = Habits | Dailys | Todos | Rewards
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data User = User
  { userId    :: Text
  , userName  :: Text
  , userStats :: UserStats
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data UserStats = UserStats
  { usHp  :: Double
  , usMp  :: Double
  , usExp :: Double
  , usGp  :: Double
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Task = Task
  { taskId        :: TaskId
  , taskText      :: Text
  , taskType      :: TaskType
  , taskCompleted :: Maybe Bool
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data ScoreResult = ScoreResult
  { srDelta :: Double     -- ^ HP/gold change
  , srDrop  :: Maybe Text -- ^ Item drop if any
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- Effect

data Habitica r where
  FetchTodos       :: Habitica [Todo]
  AddChecklistItem :: TodoId -> Text -> Habitica Text
  CreateTodo       :: Text -> Habitica TodoId
  GetUser          :: Habitica User
  ScoreTask        :: TaskId -> Direction -> Habitica ScoreResult
  GetTasks         :: TaskType -> Habitica [Task]

fetchTodos :: Member Habitica effs => Eff effs [Todo]
fetchTodos = send FetchTodos

addChecklistItem :: Member Habitica effs => TodoId -> Text -> Eff effs Text
addChecklistItem tid item = send (AddChecklistItem tid item)

createTodo :: Member Habitica effs => Text -> Eff effs TodoId
createTodo title = send (CreateTodo title)

getUser :: Member Habitica effs => Eff effs User
getUser = send GetUser

scoreTask :: Member Habitica effs => TaskId -> Direction -> Eff effs ScoreResult
scoreTask tid dir = send (ScoreTask tid dir)

getTasks :: Member Habitica effs => TaskType -> Eff effs [Task]
getTasks tt = send (GetTasks tt)

-- Stub runner (errors on call)

runHabiticaStub :: Member Log effs => Eff (Habitica ': effs) a -> Eff effs a
runHabiticaStub = interpret $ \case
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

dirText :: Direction -> Text
dirText Up = "Up"
dirText Down = "Down"

taskTypeText :: TaskType -> Text
taskTypeText Habits = "Habits"
taskTypeText Dailys = "Dailys"
taskTypeText Todos = "Todos"
taskTypeText Rewards = "Rewards"
