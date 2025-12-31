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
import Effectful
import Effectful.Dispatch.Dynamic

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

data Habitica :: Effect where
  FetchTodos       :: Habitica m [Todo]
  AddChecklistItem :: TodoId -> Text -> Habitica m Text
  CreateTodo       :: Text -> Habitica m TodoId
  GetUser          :: Habitica m User
  ScoreTask        :: TaskId -> Direction -> Habitica m ScoreResult
  GetTasks         :: TaskType -> Habitica m [Task]

type instance DispatchOf Habitica = 'Dynamic

fetchTodos :: Habitica :> es => Eff es [Todo]
fetchTodos = send FetchTodos

addChecklistItem :: Habitica :> es => TodoId -> Text -> Eff es Text
addChecklistItem tid item = send (AddChecklistItem tid item)

createTodo :: Habitica :> es => Text -> Eff es TodoId
createTodo title = send (CreateTodo title)

getUser :: Habitica :> es => Eff es User
getUser = send GetUser

scoreTask :: Habitica :> es => TaskId -> Direction -> Eff es ScoreResult
scoreTask tid dir = send (ScoreTask tid dir)

getTasks :: Habitica :> es => TaskType -> Eff es [Task]
getTasks tt = send (GetTasks tt)

-- Stub runner (errors on call)

runHabiticaStub :: (IOE :> es, Log :> es) => Eff (Habitica : es) a -> Eff es a
runHabiticaStub = interpret $ \_ -> \case
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
