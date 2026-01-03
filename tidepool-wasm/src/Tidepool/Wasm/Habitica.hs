{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Type-safe Habitica API operations.
--
-- This module provides a typed interface to the Habitica API, replacing
-- the error-prone raw string + JSON interface.
--
-- = Problem
--
-- The raw API takes operation names and payloads as untyped values:
--
-- @
-- habitica "GetTasks" $ object ["taskType" .= "dailys"]
-- @
--
-- A typo like @"type"@ instead of @"taskType"@ causes a runtime error.
--
-- = Solution
--
-- This module provides a GADT where each operation specifies its exact
-- payload and return type:
--
-- @
-- habitica (GetTasks Dailys)  -- Returns [HabiticaTask]
-- habitica (CreateTodo title) -- Returns TodoId
-- @
--
-- If it compiles, the payload shape is correct.
module Tidepool.Wasm.Habitica
  ( -- * Domain Types
    TaskType(..)
  , Direction(..)
  , TaskId(..)
  , TodoId(..)
  , ChecklistItemId(..)

    -- * Response Types
  , UserInfo(..)
  , UserStats(..)
  , HabiticaTask(..)
  , FetchedTodo(..)
  , FetchedChecklistItem(..)
  , ScoreResult(..)

    -- * Operations GADT
  , HabiticaOp(..)

    -- * Typed Effect
  , habitica
  ) where

import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Coroutine (Yield, yield)
import Data.Aeson
  ( FromJSON(..)
  , Result(..)
  , ToJSON(..)
  , Value(..)
  , fromJSON
  , object
  , withObject
  , (.=)
  , (.:)
  , (.:?)
  )
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Tidepool.Wasm.WireTypes (SerializableEffect(..), EffectResult(..))
import Tidepool.Wasm.Error
  ( WasmError
  , effectFailed
  , parseFailed
  , emptyResult
  )


-- ════════════════════════════════════════════════════════════════════════════
-- DOMAIN TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Habitica task types.
data TaskType
  = Habits
  | Dailys
  | Todos
  | Rewards
  deriving stock (Eq, Show, Generic)

-- | Score direction for tasks.
data Direction
  = Up
  | Down
  deriving stock (Eq, Show, Generic)

-- | Opaque task ID from Habitica.
newtype TaskId = TaskId { unTaskId :: Text }
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

-- | Opaque todo ID from Habitica.
newtype TodoId = TodoId { unTodoId :: Text }
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

-- | Opaque checklist item ID from Habitica.
newtype ChecklistItemId = ChecklistItemId { unChecklistItemId :: Text }
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- RESPONSE TYPES
-- Matches TypeScript handler responses exactly.
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

taskTypeToText :: TaskType -> Text
taskTypeToText = \case
  Habits  -> "habits"
  Dailys  -> "dailys"
  Todos   -> "todos"
  Rewards -> "rewards"

parseTaskType :: Text -> Parser TaskType
parseTaskType t = case T.toLower t of
  "habits"  -> pure Habits
  "dailys"  -> pure Dailys
  "todos"   -> pure Todos
  "rewards" -> pure Rewards
  other     -> fail $ "Unknown task type: " <> T.unpack other

directionToText :: Direction -> Text
directionToText = \case
  Up   -> "up"
  Down -> "down"


-- ════════════════════════════════════════════════════════════════════════════
-- OPERATIONS GADT
-- ════════════════════════════════════════════════════════════════════════════

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


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT IMPLEMENTATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Execute a typed Habitica operation.
--
-- This function encodes the operation to the wire format, yields to TypeScript
-- for execution, and decodes the response to the expected type.
--
-- Returns @Left WasmError@ on failure, @Right a@ on success.
--
-- Example:
--
-- @
-- result <- habitica FetchTodos
-- case result of
--   Left err -> -- handle error
--   Right todos -> -- use todos
-- @
habitica :: Member (Yield SerializableEffect EffectResult) effs
         => HabiticaOp a
         -> Eff effs (Either WasmError a)
habitica op = do
  let (opName, payload) = encodeOp op
      eff = EffHabitica opName payload
  result <- yield eff (id @EffectResult)
  pure $ case result of
    ResSuccess (Just v) -> decodeResult eff op v
    ResSuccess Nothing  -> Left $ emptyResult eff "Habitica API response"
    ResError msg _      -> Left $ effectFailed eff msg

-- | Encode an operation to wire format (operation name + JSON payload).
encodeOp :: HabiticaOp a -> (Text, Value)
encodeOp = \case
  GetUser ->
    ("GetUser", object [])

  GetTasks taskType ->
    ("GetTasks", object ["taskType" .= taskTypeToText taskType])

  FetchTodos ->
    ("FetchTodos", object [])

  ScoreTask taskId direction ->
    ("ScoreTask", object
      [ "taskId" .= taskId
      , "direction" .= directionToText direction
      ])

  CreateTodo title ->
    ("CreateTodo", object ["title" .= title])

  AddChecklistItem todoId item ->
    ("AddChecklistItem", object
      [ "todoId" .= todoId
      , "item" .= item
      ])

-- | Decode the result based on the operation type.
decodeResult :: SerializableEffect -> HabiticaOp a -> Value -> Either WasmError a
decodeResult eff op v = case op of
  GetUser -> parse @UserInfo "UserInfo" v
  GetTasks _ -> parse @[HabiticaTask] "[HabiticaTask]" v
  FetchTodos -> parse @[FetchedTodo] "[FetchedTodo]" v
  ScoreTask _ _ -> parse @ScoreResult "ScoreResult" v
  CreateTodo _ -> parseTodoId v
  AddChecklistItem _ _ -> parseChecklistItemId v
  where
    parse :: FromJSON a => Text -> Value -> Either WasmError a
    parse typeName val = case fromJSON val of
      Success a -> Right a
      Error err -> Left $ parseFailed eff typeName val (T.pack err)

    -- CreateTodo returns { unTodoId: "..." }
    parseTodoId :: Value -> Either WasmError TodoId
    parseTodoId val = case fromJSON val of
      Success (tid :: TodoIdResponse) -> Right (TodoId tid.tirTodoId)
      Error err -> Left $ parseFailed eff "TodoId" val (T.pack err)

    -- AddChecklistItem returns just the string ID
    parseChecklistItemId :: Value -> Either WasmError ChecklistItemId
    parseChecklistItemId val = case fromJSON val of
      Success (t :: Text) -> Right (ChecklistItemId t)
      Error err -> Left $ parseFailed eff "ChecklistItemId" val (T.pack err)

-- Helper newtype for parsing CreateTodo response
newtype TodoIdResponse = TodoIdResponse { tirTodoId :: Text }

instance FromJSON TodoIdResponse where
  parseJSON = withObject "TodoIdResponse" $ \v ->
    TodoIdResponse <$> v .: "unTodoId"
