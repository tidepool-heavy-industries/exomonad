{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Tests for Habitica response type parsing.
--
-- These tests validate that the Haskell FromJSON instances correctly parse
-- the JSON structures produced by the TypeScript handler (deploy/src/handlers/habitica.ts).
--
-- This catches protocol mismatches before they become runtime errors.
module HabiticaTypesSpec (spec) where

import Test.Hspec
import Data.Aeson (decode, encode, object, (.=))
import Data.Aeson.QQ (aesonQQ)
import Data.Maybe (isJust)
import qualified Data.Text as T

import Tidepool.Wasm.Habitica
  ( UserInfo(..)
  , UserStats(..)
  , HabiticaTask(..)
  , FetchedTodo(..)
  , FetchedChecklistItem(..)
  , ScoreResult(..)
  , TaskType(..)
  , TaskId(..)
  , TodoId(..)
  )


-- | Helper to parse a task type string through a minimal HabiticaTask JSON.
parseTaskType :: T.Text -> Maybe TaskType
parseTaskType t = fmap (.htType) $ (decode $ encode $ object
  [ "taskId" .= ("x" :: T.Text)
  , "taskText" .= ("x" :: T.Text)
  , "taskType" .= t
  ] :: Maybe HabiticaTask)


spec :: Spec
spec = do
  userInfoSpec
  userStatsSpec
  habiticaTaskSpec
  fetchedTodoSpec
  scoreResultSpec


-- ════════════════════════════════════════════════════════════════════════════
-- UserInfo (GetUser response)
-- ════════════════════════════════════════════════════════════════════════════

userInfoSpec :: Spec
userInfoSpec = describe "UserInfo" $ do

  it "parses GetUser response from TypeScript handler" $ do
    -- This JSON matches what handleHabitica returns for GetUser
    let json = [aesonQQ|{
      "userId": "abc123",
      "userName": "TestUser",
      "userStats": {
        "usHp": 50.0,
        "usMp": 100.0,
        "usExp": 1500.0,
        "usGp": 250.5
      }
    }|]
    let parsed = decode (encode json) :: Maybe UserInfo
    parsed `shouldSatisfy` isJust
    case parsed of
      Just ui -> do
        ui.uiUserId `shouldBe` "abc123"
        ui.uiUserName `shouldBe` "TestUser"
        ui.uiStats.usHp `shouldBe` 50.0
        ui.uiStats.usMp `shouldBe` 100.0
        ui.uiStats.usExp `shouldBe` 1500.0
        ui.uiStats.usGp `shouldBe` 250.5
      Nothing -> expectationFailure "Expected successful parse"

  it "parses empty username" $ do
    let json = [aesonQQ|{
      "userId": "xyz",
      "userName": "",
      "userStats": {"usHp": 0, "usMp": 0, "usExp": 0, "usGp": 0}
    }|]
    let parsed = decode (encode json) :: Maybe UserInfo
    parsed `shouldSatisfy` isJust


-- ════════════════════════════════════════════════════════════════════════════
-- UserStats
-- ════════════════════════════════════════════════════════════════════════════

userStatsSpec :: Spec
userStatsSpec = describe "UserStats" $ do

  it "parses integer values as doubles" $ do
    -- TypeScript may send integer values, Haskell expects Double
    let json = [aesonQQ|{
      "usHp": 50,
      "usMp": 100,
      "usExp": 1500,
      "usGp": 250
    }|]
    let parsed = decode (encode json) :: Maybe UserStats
    parsed `shouldSatisfy` isJust

  it "parses floating point values" $ do
    let json = [aesonQQ|{
      "usHp": 49.5,
      "usMp": 99.9,
      "usExp": 1500.75,
      "usGp": 250.123
    }|]
    let parsed = decode (encode json) :: Maybe UserStats
    parsed `shouldSatisfy` isJust


-- ════════════════════════════════════════════════════════════════════════════
-- HabiticaTask (GetTasks response)
-- ════════════════════════════════════════════════════════════════════════════

habiticaTaskSpec :: Spec
habiticaTaskSpec = describe "HabiticaTask" $ do

  it "parses GetTasks response item from TypeScript handler" $ do
    -- This JSON matches what handleHabitica returns for GetTasks
    let json = [aesonQQ|{
      "taskId": "task-123-abc",
      "taskText": "Complete daily review",
      "taskType": "Dailys",
      "taskCompleted": false
    }|]
    let parsed = decode (encode json) :: Maybe HabiticaTask
    parsed `shouldSatisfy` isJust
    case parsed of
      Just task -> do
        task.htTaskId.unTaskId `shouldBe` "task-123-abc"
        task.htText `shouldBe` "Complete daily review"
        task.htType `shouldBe` Dailys
        task.htCompleted `shouldBe` Just False
      Nothing -> expectationFailure "Expected successful parse"

  it "parses all task types" $ do
    parseTaskType "Habits" `shouldBe` Just Habits
    parseTaskType "Dailys" `shouldBe` Just Dailys
    parseTaskType "Todos" `shouldBe` Just Todos
    parseTaskType "Rewards" `shouldBe` Just Rewards

  it "parses task types case-insensitively" $ do
    parseTaskType "habits" `shouldBe` Just Habits
    parseTaskType "DAILYS" `shouldBe` Just Dailys
    parseTaskType "todos" `shouldBe` Just Todos
    parseTaskType "rewards" `shouldBe` Just Rewards

  it "parses null taskCompleted as Nothing" $ do
    let json = [aesonQQ|{
      "taskId": "x",
      "taskText": "x",
      "taskType": "Habits",
      "taskCompleted": null
    }|]
    let parsed = decode (encode json) :: Maybe HabiticaTask
    case parsed of
      Just task -> task.htCompleted `shouldBe` Nothing
      Nothing -> expectationFailure "Expected successful parse"

  it "parses missing taskCompleted as Nothing" $ do
    let json = [aesonQQ|{
      "taskId": "x",
      "taskText": "x",
      "taskType": "Habits"
    }|]
    let parsed = decode (encode json) :: Maybe HabiticaTask
    case parsed of
      Just task -> task.htCompleted `shouldBe` Nothing
      Nothing -> expectationFailure "Expected successful parse"

  it "parses list of tasks" $ do
    let json = [aesonQQ|[
      {"taskId": "1", "taskText": "Task 1", "taskType": "Dailys", "taskCompleted": true},
      {"taskId": "2", "taskText": "Task 2", "taskType": "Todos", "taskCompleted": false}
    ]|]
    let parsed = decode (encode json) :: Maybe [HabiticaTask]
    parsed `shouldSatisfy` isJust
    case parsed of
      Just tasks -> length tasks `shouldBe` 2
      Nothing -> expectationFailure "Expected successful parse"


-- ════════════════════════════════════════════════════════════════════════════
-- FetchedTodo (FetchTodos response)
-- ════════════════════════════════════════════════════════════════════════════

fetchedTodoSpec :: Spec
fetchedTodoSpec = describe "FetchedTodo" $ do

  it "parses FetchTodos response item from TypeScript handler" $ do
    -- This JSON matches what handleHabitica returns for FetchTodos
    let json = [aesonQQ|{
      "todoId": "todo-456-def",
      "todoTitle": "Write tests",
      "todoChecklist": [
        {"checklistId": "cl-1", "checklistText": "Unit tests", "checklistDone": true},
        {"checklistId": "cl-2", "checklistText": "Integration tests", "checklistDone": false}
      ],
      "todoCompleted": false
    }|]
    let parsed = decode (encode json) :: Maybe FetchedTodo
    parsed `shouldSatisfy` isJust
    case parsed of
      Just todo -> do
        todo.ftTodoId.unTodoId `shouldBe` "todo-456-def"
        todo.ftTitle `shouldBe` "Write tests"
        length todo.ftChecklist `shouldBe` 2
        todo.ftCompleted `shouldBe` False

        case todo.ftChecklist of
          (item1:_) -> do
            item1.fciId `shouldBe` "cl-1"
            item1.fciText `shouldBe` "Unit tests"
            item1.fciCompleted `shouldBe` True
          [] -> expectationFailure "Expected non-empty checklist"
      Nothing -> expectationFailure "Expected successful parse"

  it "parses todo with empty checklist" $ do
    let json = [aesonQQ|{
      "todoId": "x",
      "todoTitle": "Simple todo",
      "todoChecklist": [],
      "todoCompleted": true
    }|]
    let parsed = decode (encode json) :: Maybe FetchedTodo
    parsed `shouldSatisfy` isJust
    case parsed of
      Just todo -> do
        todo.ftChecklist `shouldBe` []
        todo.ftCompleted `shouldBe` True
      Nothing -> expectationFailure "Expected successful parse"

  it "parses list of todos" $ do
    let json = [aesonQQ|[
      {"todoId": "1", "todoTitle": "A", "todoChecklist": [], "todoCompleted": false},
      {"todoId": "2", "todoTitle": "B", "todoChecklist": [], "todoCompleted": true}
    ]|]
    let parsed = decode (encode json) :: Maybe [FetchedTodo]
    parsed `shouldSatisfy` isJust
    case parsed of
      Just todos -> length todos `shouldBe` 2
      Nothing -> expectationFailure "Expected successful parse"


-- ════════════════════════════════════════════════════════════════════════════
-- ScoreResult (ScoreTask response)
-- ════════════════════════════════════════════════════════════════════════════

scoreResultSpec :: Spec
scoreResultSpec = describe "ScoreResult" $ do

  it "parses ScoreTask response from TypeScript handler" $ do
    -- This JSON matches what handleHabitica returns for ScoreTask
    let json = [aesonQQ|{
      "srDelta": 1.5,
      "srDrop": "Sword of Destiny"
    }|]
    let parsed = decode (encode json) :: Maybe ScoreResult
    parsed `shouldSatisfy` isJust
    case parsed of
      Just result -> do
        result.srDelta `shouldBe` 1.5
        result.srDrop `shouldBe` Just "Sword of Destiny"
      Nothing -> expectationFailure "Expected successful parse"

  it "parses null drop as Nothing" $ do
    let json = [aesonQQ|{
      "srDelta": -0.5,
      "srDrop": null
    }|]
    let parsed = decode (encode json) :: Maybe ScoreResult
    case parsed of
      Just result -> do
        result.srDelta `shouldBe` (-0.5)
        result.srDrop `shouldBe` Nothing
      Nothing -> expectationFailure "Expected successful parse"

  it "parses missing drop as Nothing" $ do
    let json = [aesonQQ|{
      "srDelta": 2.0
    }|]
    let parsed = decode (encode json) :: Maybe ScoreResult
    case parsed of
      Just result -> result.srDrop `shouldBe` Nothing
      Nothing -> expectationFailure "Expected successful parse"

  it "parses integer delta as double" $ do
    let json = [aesonQQ|{"srDelta": 3}|]
    let parsed = decode (encode json) :: Maybe ScoreResult
    case parsed of
      Just result -> result.srDelta `shouldBe` 3.0
      Nothing -> expectationFailure "Expected successful parse"
