{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Tests for Habitica response type parsing.
--
-- These tests validate that the Haskell FromJSON instances correctly parse
-- the JSON structures produced by the Habitica API.
--
-- This catches protocol mismatches before they become runtime errors.
module ResponseSpec (spec) where

import Data.Aeson (decode, encode, object, (.=))
import Data.Aeson.QQ (aesonQQ)
import Data.Maybe (isJust)
import Data.Text qualified as T
import ExoMonad.Habitica
  ( FetchedChecklistItem (..),
    FetchedTodo (..),
    HabiticaTask (..),
    ScoreResult (..),
    TaskId (..),
    TaskType (..),
    TodoId (..),
    UserInfo (..),
    UserStats (..),
  )
import Test.Hspec

-- | Helper to parse a task type string through a minimal HabiticaTask JSON.
parseTaskType :: T.Text -> Maybe TaskType
parseTaskType t =
  fmap (.taskType) $
    ( decode $
        encode $
          object
            [ "id" .= ("x" :: T.Text),
              "text" .= ("x" :: T.Text),
              "type" .= t
            ] ::
        Maybe HabiticaTask
    )

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
  it "parses GetUser response" $ do
    -- This JSON matches what handleHabitica returns for GetUser
    let json =
          [aesonQQ|{
      "userId": "abc123",
      "userName": "TestUser",
      "stats": {
        "hp": 50.0,
        "mp": 100.0,
        "exp": 1500.0,
        "gp": 250.5
      }
    }|]
    let parsed = decode (encode json) :: Maybe UserInfo
    parsed `shouldSatisfy` isJust
    case parsed of
      Just ui -> do
        ui.userId `shouldBe` "abc123"
        ui.userName `shouldBe` "TestUser"
        ui.stats.hp `shouldBe` 50.0
        ui.stats.mp `shouldBe` 100.0
        ui.stats.exp `shouldBe` 1500.0
        ui.stats.gp `shouldBe` 250.5
      Nothing -> expectationFailure "Expected successful parse"

  it "parses empty username" $ do
    let json =
          [aesonQQ|{
      "userId": "xyz",
      "userName": "",
      "stats": {"hp": 0, "mp": 0, "exp": 0, "gp": 0}
    }|]
    let parsed = decode (encode json) :: Maybe UserInfo
    parsed `shouldSatisfy` isJust

-- ════════════════════════════════════════════════════════════════════════════
-- UserStats
-- ════════════════════════════════════════════════════════════════════════════

userStatsSpec :: Spec
userStatsSpec = describe "UserStats" $ do
  it "parses integer values as doubles" $ do
    let json =
          [aesonQQ|{
      "hp": 50,
      "mp": 100,
      "exp": 1500,
      "gp": 250
    }|]
    let parsed = decode (encode json) :: Maybe UserStats
    parsed `shouldSatisfy` isJust

  it "parses floating point values" $ do
    let json =
          [aesonQQ|{
      "hp": 49.5,
      "mp": 99.9,
      "exp": 1500.75,
      "gp": 250.123
    }|]
    let parsed = decode (encode json) :: Maybe UserStats
    parsed `shouldSatisfy` isJust

-- ════════════════════════════════════════════════════════════════════════════
-- HabiticaTask (GetTasks response)
-- ════════════════════════════════════════════════════════════════════════════

habiticaTaskSpec :: Spec
habiticaTaskSpec = describe "HabiticaTask" $ do
  it "parses GetTasks response item" $ do
    -- This JSON matches what handleHabitica returns for GetTasks
    let json =
          [aesonQQ|{
      "id": "task-123-abc",
      "text": "Complete daily review",
      "type": "Dailys",
      "completed": false
    }|]
    let parsed = decode (encode json) :: Maybe HabiticaTask
    parsed `shouldSatisfy` isJust
    case parsed of
      Just task -> do
        task.id.unTaskId `shouldBe` "task-123-abc"
        task.text `shouldBe` "Complete daily review"
        task.taskType `shouldBe` Dailys
        task.completed `shouldBe` Just False
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
    let json =
          [aesonQQ|{
      "id": "x",
      "text": "x",
      "type": "Habits",
      "completed": null
    }|]
    let parsed = decode (encode json) :: Maybe HabiticaTask
    case parsed of
      Just task -> task.completed `shouldBe` Nothing
      Nothing -> expectationFailure "Expected successful parse"

  it "parses missing taskCompleted as Nothing" $ do
    let json =
          [aesonQQ|{
      "id": "x",
      "text": "x",
      "type": "Habits"
    }|]
    let parsed = decode (encode json) :: Maybe HabiticaTask
    case parsed of
      Just task -> task.completed `shouldBe` Nothing
      Nothing -> expectationFailure "Expected successful parse"

  it "parses list of tasks" $ do
    let json =
          [aesonQQ|[
      {"id": "1", "text": "Task 1", "type": "Dailys", "completed": true},
      {"id": "2", "text": "Task 2", "type": "Todos", "completed": false}
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
  it "parses FetchTodos response item" $ do
    -- This JSON matches what handleHabitica returns for FetchTodos
    let json =
          [aesonQQ|{
      "id": "todo-456-def",
      "title": "Write tests",
      "checklist": [
        {"id": "cl-1", "text": "Unit tests", "completed": true},
        {"id": "cl-2", "text": "Integration tests", "completed": false}
      ],
      "completed": false
    }|]
    let parsed = decode (encode json) :: Maybe FetchedTodo
    parsed `shouldSatisfy` isJust
    case parsed of
      Just todo -> do
        todo.id.unTodoId `shouldBe` "todo-456-def"
        todo.title `shouldBe` "Write tests"
        length todo.checklist `shouldBe` 2
        todo.completed `shouldBe` False

        case todo.checklist of
          (item1 : _) -> do
            item1.id `shouldBe` "cl-1"
            item1.text `shouldBe` "Unit tests"
            item1.completed `shouldBe` True
          [] -> expectationFailure "Expected non-empty checklist"
      Nothing -> expectationFailure "Expected successful parse"

  it "parses todo with empty checklist" $ do
    let json =
          [aesonQQ|{
      "id": "x",
      "title": "Simple todo",
      "checklist": [],
      "completed": true
    }|]
    let parsed = decode (encode json) :: Maybe FetchedTodo
    parsed `shouldSatisfy` isJust
    case parsed of
      Just todo -> do
        todo.checklist `shouldBe` []
        todo.completed `shouldBe` True
      Nothing -> expectationFailure "Expected successful parse"

  it "parses list of todos" $ do
    let json =
          [aesonQQ|[
      {"id": "1", "title": "A", "checklist": [], "completed": false},
      {"id": "2", "title": "B", "checklist": [], "completed": true}
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
  it "parses ScoreTask response" $ do
    -- This JSON matches what handleHabitica returns for ScoreTask
    let json =
          [aesonQQ|{
      "delta": 1.5,
      "drop": "Sword of Destiny"
    }|]
    let parsed = decode (encode json) :: Maybe ScoreResult
    parsed `shouldSatisfy` isJust
    case parsed of
      Just result -> do
        result.delta `shouldBe` 1.5
        result.drop `shouldBe` Just "Sword of Destiny"
      Nothing -> expectationFailure "Expected successful parse"

  it "parses null drop as Nothing" $ do
    let json =
          [aesonQQ|{
      "delta": -0.5,
      "drop": null
    }|]
    let parsed = decode (encode json) :: Maybe ScoreResult
    case parsed of
      Just result -> do
        result.delta `shouldBe` (-0.5)
        result.drop `shouldBe` Nothing
      Nothing -> expectationFailure "Expected successful parse"

  it "parses missing drop as Nothing" $ do
    let json =
          [aesonQQ|{
      "delta": 2.0
    }|]
    let parsed = decode (encode json) :: Maybe ScoreResult
    case parsed of
      Just result -> result.drop `shouldBe` Nothing
      Nothing -> expectationFailure "Expected successful parse"

  it "parses integer delta as double" $ do
    let json = [aesonQQ|{"delta": 3}|]
    let parsed = decode (encode json) :: Maybe ScoreResult
    case parsed of
      Just result -> result.delta `shouldBe` 3.0
      Nothing -> expectationFailure "Expected successful parse"