{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ExoMonad.Control.StopHook.Types
  ( AgentState(..)
  , WorkflowState(..)
  , WorkflowStage(..)
  , BuildResult(..)
  , BuildFailureInfo(..)
  , TemplateName
  , StopHookContext(..)
  , TestResult(..)
  ) where

import Control.Monad.Writer (Writer)
import Data.Aeson (FromJSON, ToJSON, FromJSONKey, ToJSONKey, Value)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Text.Ginger.GVal (ToGVal(..), dict, (~>), fromGVal, asText)
import Text.Ginger.Run.Type (Run)
import Text.Parsec.Pos (SourcePos)

import ExoMonad.Effects.Effector (GhPrStatusResult(..), PrComment(..))

-- | State passed through the graph
data AgentState = AgentState
  { asSessionId :: Text
  , asCwd :: FilePath
  , asBranch :: Maybe Text
  , asIssueNum :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Mutable workflow state (in State effect)
data WorkflowState = WorkflowState
  { wsGlobalStops :: Int
  , wsStageRetries :: Map WorkflowStage Int
  , wsCurrentStage :: WorkflowStage
  , wsLastBuildResult :: Maybe BuildResult
  , wsLastPRStatus :: Maybe GhPrStatusResult
  , wsLastTestResult :: Maybe TestResult
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data WorkflowStage
  = StageBuild
  | StageTest
  | StageDocs
  | StagePR
  | StageReview
  | StageComplete
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, FromJSONKey, ToJSONKey)

data BuildResult
  = BuildSuccess
  | BuildFailure BuildFailureInfo
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data BuildFailureInfo = BuildFailureInfo
  { bfiRawOutput :: Text           -- Full cabal output
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data TestResult = TestResult
  { trPassed :: Int
  , trFailed :: Int
  , trRawOutput :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

type TemplateName = Text

-- | Unified context for all stop hook templates
-- Flattened to avoid Maybe access issues in Ginger TH.
data StopHookContext = StopHookContext
  { template :: Text
  , stage :: Text
  , issue_number :: Maybe Int
  , branch :: Text
  , global_stops :: Int
  , stage_retries :: Int
  , build_failed :: Bool
  , raw_output :: Text
  -- Test context
  , tests_failed :: Bool
  , test_passed_count :: Int
  , test_failed_count :: Int
  -- PR info
  , pr_exists :: Bool
  , pr_url :: Maybe Text
  , pr_number :: Maybe Int
  , pr_review_status :: Maybe Text
  , pr_comments :: [PrComment]
  -- Doc info
  , stale_docs :: [FilePath]
  , git_dirty_files :: [FilePath]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ToGVal Instances

type GingerRun = Run SourcePos (Writer Text) Text

instance ToGVal GingerRun StopHookContext where
  toGVal ctx = dict
    [ "template" ~> template ctx
    , "stage" ~> stage ctx
    , "issue_number" ~> issue_number ctx
    , "branch" ~> branch ctx
    , "global_stops" ~> global_stops ctx
    , "stage_retries" ~> stage_retries ctx
    , "build_failed" ~> build_failed ctx
    , "raw_output" ~> raw_output ctx
    , "tests_failed" ~> tests_failed ctx
    , "test_passed_count" ~> test_passed_count ctx
    , "test_failed_count" ~> test_failed_count ctx
    , "pr_exists" ~> pr_exists ctx
    , "pr_url" ~> pr_url ctx
    , "pr_number" ~> pr_number ctx
    , "pr_review_status" ~> pr_review_status ctx
    , "pr_comments" ~> pr_comments ctx
    , "stale_docs" ~> stale_docs ctx
    , "git_dirty_files" ~> git_dirty_files ctx
    ]

instance ToGVal GingerRun PrComment where
  toGVal c = dict
    [ "author" ~> c.author
    , "body" ~> c.body
    , "path" ~> c.path
    , "line" ~> c.line
    ]