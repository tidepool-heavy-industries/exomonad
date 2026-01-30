{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module ExoMonad.Control.StopHook.Types
  ( AgentState (..),
    WorkflowState (..),
    WorkflowStage (..),
    BuildResult (..),
    BuildFailureInfo (..),
    TemplateName,
    StopHookContext (..),
    TestResult (..),
  )
where

import Control.Monad.Writer (Writer)
import Data.Aeson (ToJSON, ToJSONKey, Value)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Effects.Effector (GhPrStatusResult (..), PrComment (..))
import GHC.Generics (Generic)
import Text.Ginger.GVal (ToGVal (..), asText, dict, fromGVal, (~>))
import Text.Ginger.Run.Type (Run)
import Text.Parsec.Pos (SourcePos)

-- | State passed through the graph
data AgentState = AgentState
  { sessionId :: Text,
    cwd :: FilePath,
    branch :: Maybe Text,
    issueNum :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | Mutable workflow state (in State effect)
data WorkflowState = WorkflowState
  { globalStops :: Int,
    stageRetries :: Map WorkflowStage Int,
    currentStage :: WorkflowStage,
    lastBuildResult :: Maybe BuildResult,
    lastPRStatus :: Maybe GhPrStatusResult,
    lastTestResult :: Maybe TestResult
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data WorkflowStage
  = StageBuild
  | StageTest
  | StageDocs
  | StagePR
  | StageReview
  | StageComplete
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, ToJSONKey)

data BuildResult
  = BuildSuccess
  | BuildFailure BuildFailureInfo
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data BuildFailureInfo = BuildFailureInfo
  { rawOutput :: Text -- Full cabal output
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data TestResult = TestResult
  { passed :: Int,
    failed :: Int,
    rawOutput :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

type TemplateName = Text

-- | Unified context for all stop hook templates
-- Flattened to avoid Maybe access issues in Ginger TH.
data StopHookContext = StopHookContext
  { template :: Text,
    stage :: Text,
    issue_number :: Maybe Int,
    branch :: Text,
    global_stops :: Int,
    stage_retries :: Int,
    build_failed :: Bool,
    raw_output :: Text,
    -- Test context
    tests_failed :: Bool,
    test_passed_count :: Int,
    test_failed_count :: Int,
    -- PR info
    pr_exists :: Bool,
    pr_url :: Maybe Text,
    pr_number :: Maybe Int,
    pr_review_status :: Maybe Text,
    pr_comments :: [PrComment],
    -- Doc info
    stale_docs :: [FilePath],
    git_dirty_files :: [FilePath]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- ToGVal Instances

type GingerRun = Run SourcePos (Writer Text) Text

instance ToGVal GingerRun StopHookContext where
  toGVal ctx =
    dict
      [ "template" ~> ctx.template,
        "stage" ~> ctx.stage,
        "issue_number" ~> ctx.issue_number,
        "branch" ~> ctx.branch,
        "global_stops" ~> ctx.global_stops,
        "stage_retries" ~> ctx.stage_retries,
        "build_failed" ~> ctx.build_failed,
        "raw_output" ~> ctx.raw_output,
        "tests_failed" ~> ctx.tests_failed,
        "test_passed_count" ~> ctx.test_passed_count,
        "test_failed_count" ~> ctx.test_failed_count,
        "pr_exists" ~> ctx.pr_exists,
        "pr_url" ~> ctx.pr_url,
        "pr_number" ~> ctx.pr_number,
        "pr_review_status" ~> ctx.pr_review_status,
        "pr_comments" ~> ctx.pr_comments,
        "stale_docs" ~> ctx.stale_docs,
        "git_dirty_files" ~> ctx.git_dirty_files
      ]

instance ToGVal GingerRun PrComment where
  toGVal c =
    dict
      [ "author" ~> c.author,
        "body" ~> c.body,
        "path" ~> c.path,
        "line" ~> c.line
      ]
