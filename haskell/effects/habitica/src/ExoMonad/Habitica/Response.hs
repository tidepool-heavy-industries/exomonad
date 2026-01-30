{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Response types from Habitica API.
--
-- These types have custom FromJSON instances that match the JSON structure
-- produced by the TypeScript handler (deploy/src/handlers/habitica.ts).
module ExoMonad.Habitica.Response
  ( -- * User Types
    UserInfo (..),
    UserStats (..),

    -- * Task Types
    HabiticaTask (..),

    -- * Todo Types
    FetchedTodo (..),
    FetchedChecklistItem (..),

    -- * Score Types
    ScoreResult (..),
  )
where

import Data.Aeson
  ( FromJSON (..),
    withObject,
    (.:),
    (.:?),
  )
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Habitica.Types (TaskId (..), TaskType (..), TodoId (..))
import GHC.Generics (Generic)

-- ════════════════════════════════════════════════════════════════════════════
-- USER TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | User info returned by GetUser.
data UserInfo = UserInfo
  { userId :: Text,
    userName :: Text,
    stats :: UserStats
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

-- | User stats (HP, MP, EXP, GP).
data UserStats = UserStats
  { hp :: Double,
    mp :: Double,
    exp :: Double,
    gp :: Double
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

-- ════════════════════════════════════════════════════════════════════════════
-- TASK TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Task returned by GetTasks.
data HabiticaTask = HabiticaTask
  { id :: TaskId,
    text :: Text,
    taskType :: TaskType,
    completed :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON HabiticaTask where
  parseJSON = withObject "HabiticaTask" $ \v ->
    HabiticaTask
      <$> fmap TaskId (v .: "id")
      <*> v .: "text"
      <*> (v .: "type" >>= parseTaskType)
      <*> v .:? "completed"

-- ════════════════════════════════════════════════════════════════════════════
-- TODO TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Todo returned by FetchTodos.
data FetchedTodo = FetchedTodo
  { id :: TodoId,
    title :: Text,
    checklist :: [FetchedChecklistItem],
    completed :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

-- | Checklist item within a todo.
data FetchedChecklistItem = FetchedChecklistItem
  { id :: Text,
    text :: Text,
    completed :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

-- ════════════════════════════════════════════════════════════════════════════
-- SCORE TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Result of scoring a task.
data ScoreResult = ScoreResult
  { delta :: Double,
    drop :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

parseTaskType :: Text -> Parser TaskType
parseTaskType t = case T.toLower t of
  "habits" -> pure Habits
  "dailys" -> pure Dailys
  "todos" -> pure Todos
  "rewards" -> pure Rewards
  other -> fail $ "Unknown task type: " <> T.unpack other
