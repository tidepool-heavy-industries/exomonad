{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | WASM-specific Habitica operations.
--
-- This module provides the WASM yield/resume interface for Habitica operations.
-- Types are re-exported from "Tidepool.Habitica".
--
-- = Usage
--
-- @
-- todos <- habitica FetchTodos
-- todoId <- habitica (CreateTodo "Buy groceries")
-- _ <- habitica (AddChecklistItem todoId "Milk")
-- @
module Tidepool.Wasm.Habitica
  ( -- * Re-exported Types
    module Tidepool.Habitica

    -- * WASM Effect
  , habitica
  ) where

import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Coroutine (Yield, yield)
import Data.Aeson
  ( FromJSON(..)
  , Result(..)
  , Value(..)
  , fromJSON
  , object
  , withObject
  , (.=)
  , (.:)
  )
import Data.Text (Text)
import qualified Data.Text as T

import Tidepool.Habitica
import Tidepool.Wasm.WireTypes (SerializableEffect(..), EffectResult(..))
import Tidepool.Wasm.Error
  ( WasmError
  , effectFailed
  , parseFailed
  , emptyResult
  )


-- ════════════════════════════════════════════════════════════════════════════
-- WASM EFFECT IMPLEMENTATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Execute a typed Habitica operation via WASM yield/resume.
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
    ResError msg        -> Left $ effectFailed eff msg


-- ════════════════════════════════════════════════════════════════════════════
-- WIRE FORMAT ENCODING
-- ════════════════════════════════════════════════════════════════════════════

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


-- ════════════════════════════════════════════════════════════════════════════
-- WIRE FORMAT DECODING
-- ════════════════════════════════════════════════════════════════════════════

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


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

taskTypeToText :: TaskType -> Text
taskTypeToText = \case
  Habits  -> "habits"
  Dailys  -> "dailys"
  Todos   -> "todos"
  Rewards -> "rewards"

directionToText :: Direction -> Text
directionToText = \case
  Up   -> "up"
  Down -> "down"

-- Helper newtype for parsing CreateTodo response
newtype TodoIdResponse = TodoIdResponse { tirTodoId :: Text }

instance FromJSON TodoIdResponse where
  parseJSON = withObject "TodoIdResponse" $ \v ->
    TodoIdResponse <$> v .: "unTodoId"
