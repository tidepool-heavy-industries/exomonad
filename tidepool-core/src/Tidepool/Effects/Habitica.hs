-- | Habitica integration effect
module Tidepool.Effects.Habitica
  ( -- * Effect
    Habitica(..)
  , fetchTodos
  , addChecklistItem
  , createTodo

    -- * Types
  , Todo(..)
  , TodoId(..)
  , ChecklistItem(..)

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

-- Effect

data Habitica :: Effect where
  FetchTodos       :: Habitica m [Todo]
  AddChecklistItem :: TodoId -> Text -> Habitica m Text
  CreateTodo       :: Text -> Habitica m TodoId

type instance DispatchOf Habitica = 'Dynamic

fetchTodos :: Habitica :> es => Eff es [Todo]
fetchTodos = send FetchTodos

addChecklistItem :: Habitica :> es => TodoId -> Text -> Eff es Text
addChecklistItem tid item = send (AddChecklistItem tid item)

createTodo :: Habitica :> es => Text -> Eff es TodoId
createTodo title = send (CreateTodo title)

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
