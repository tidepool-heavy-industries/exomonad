-- | Response types from Habitica API.
--
-- These types have custom FromJSON instances that match the JSON structure
-- produced by the TypeScript handler (deploy/src/handlers/habitica.ts).
--
-- The field names use prefixed Haskell conventions while parsing from
-- the TypeScript handler's JSON field names.
module ExoMonad.Habitica.Response
  ( -- * User Types
    UserInfo(..)
  , UserStats(..)

    -- * Task Types
  , HabiticaTask(..)

    -- * Todo Types
  , FetchedTodo(..)
  , FetchedChecklistItem(..)

    -- * Score Types
  , ScoreResult(..)
  ) where

import Data.Aeson
  ( FromJSON(..)
  , withObject
  , (.:)
  , (.:?)
  )
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import ExoMonad.Habitica.Types (TaskId(..), TodoId(..), TaskType(..))


-- ════════════════════════════════════════════════════════════════════════════
-- USER TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | User info returned by GetUser.
data UserInfo = UserInfo
  { uiUserId   :: Text
  , uiUserName :: Text
  , uiStats    :: UserStats
  } deriving stock (Eq, Show, Generic)

instance FromJSON UserInfo where
  parseJSON = withObject "UserInfo" $ \v -> UserInfo
    <$> v .: "userId"
    <*> v .: "userName"
    <*> v .: "userStats"

-- | User stats (HP, MP, EXP, GP).
data UserStats = UserStats
  { usHp  :: Double
  , usMp  :: Double
  , usExp :: Double
  , usGp  :: Double
  } deriving stock (Eq, Show, Generic)

instance FromJSON UserStats where
  parseJSON = withObject "UserStats" $ \v -> UserStats
    <$> v .: "usHp"
    <*> v .: "usMp"
    <*> v .: "usExp"
    <*> v .: "usGp"


-- ════════════════════════════════════════════════════════════════════════════
-- TASK TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Task returned by GetTasks.
data HabiticaTask = HabiticaTask
  { htTaskId    :: TaskId
  , htText      :: Text
  , htType      :: TaskType
  , htCompleted :: Maybe Bool
  } deriving stock (Eq, Show, Generic)

instance FromJSON HabiticaTask where
  parseJSON = withObject "HabiticaTask" $ \v -> HabiticaTask
    <$> fmap TaskId (v .: "taskId")
    <*> v .: "taskText"
    <*> (v .: "taskType" >>= parseTaskType)
    <*> v .:? "taskCompleted"


-- ════════════════════════════════════════════════════════════════════════════
-- TODO TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Todo returned by FetchTodos.
data FetchedTodo = FetchedTodo
  { ftTodoId    :: TodoId
  , ftTitle     :: Text
  , ftChecklist :: [FetchedChecklistItem]
  , ftCompleted :: Bool
  } deriving stock (Eq, Show, Generic)

instance FromJSON FetchedTodo where
  parseJSON = withObject "FetchedTodo" $ \v -> FetchedTodo
    <$> fmap TodoId (v .: "todoId")
    <*> v .: "todoTitle"
    <*> v .: "todoChecklist"
    <*> v .: "todoCompleted"

-- | Checklist item within a todo.
data FetchedChecklistItem = FetchedChecklistItem
  { fciId        :: Text
  , fciText      :: Text
  , fciCompleted :: Bool
  } deriving stock (Eq, Show, Generic)

instance FromJSON FetchedChecklistItem where
  parseJSON = withObject "FetchedChecklistItem" $ \v -> FetchedChecklistItem
    <$> v .: "checklistId"
    <*> v .: "checklistText"
    <*> v .: "checklistDone"


-- ════════════════════════════════════════════════════════════════════════════
-- SCORE TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Result of scoring a task.
data ScoreResult = ScoreResult
  { srDelta :: Double
  , srDrop  :: Maybe Text
  } deriving stock (Eq, Show, Generic)

instance FromJSON ScoreResult where
  parseJSON = withObject "ScoreResult" $ \v -> ScoreResult
    <$> v .: "srDelta"
    <*> v .:? "srDrop"


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

parseTaskType :: Text -> Parser TaskType
parseTaskType t = case T.toLower t of
  "habits"  -> pure Habits
  "dailys"  -> pure Dailys
  "todos"   -> pure Todos
  "rewards" -> pure Rewards
  other     -> fail $ "Unknown task type: " <> T.unpack other
