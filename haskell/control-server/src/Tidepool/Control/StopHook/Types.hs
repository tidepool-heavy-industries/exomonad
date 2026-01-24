{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tidepool.Control.StopHook.Types
  ( AgentState(..)
  , WorkflowState(..)
  , WorkflowStage(..)
  , BuildResult(..)
  , BuildFailureInfo(..)
  , GHCError(..)
  , GHCWarning(..)
  , GHCErrorType(..)
  , TypeErrorInfo(..)
  , ScopeErrorInfo(..)
  , Severity(..)
  , TemplateName
  , StopHookContext(..)
  ) where

import Control.Monad.Writer (Writer)
import Data.Aeson (FromJSON, ToJSON, FromJSONKey, ToJSONKey, Value)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Text.Ginger.GVal (ToGVal(..), dict, (~>))
import Text.Ginger.Run.Type (Run)
import Text.Parsec.Pos (SourcePos)

-- | State passed through the graph
data AgentState = AgentState
  { asSessionId :: Text
  , asCwd :: FilePath
  , asBranch :: Maybe Text
  , asBeadId :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Mutable workflow state (in State effect)
data WorkflowState = WorkflowState
  { wsGlobalStops :: Int
  , wsStageRetries :: Map WorkflowStage Int
  , wsCurrentStage :: WorkflowStage
  , wsLastBuildResult :: Maybe BuildResult
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
  , bfiErrors :: [GHCError]        -- Parsed errors
  , bfiWarnings :: [GHCWarning]    -- Parsed warnings
  , bfiErrorCount :: Int
  , bfiWarningCount :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data GHCError = GHCError
  { geFile :: FilePath
  , geLine :: Int
  , geColumn :: Int
  , geMessage :: Text
  , geErrorType :: GHCErrorType
  , geSeverity :: Severity
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | For now, warnings use the same structure as errors
type GHCWarning = GHCError

data Severity = ErrorSeverity | WarningSeverity
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data GHCErrorType
  = TypeError TypeErrorInfo
  | ScopeError ScopeErrorInfo
  | ParseError Text
  | OtherError Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data TypeErrorInfo
  = TypeMismatch { teExpected :: Text, teActual :: Text }
  | AmbiguousType Text
  | MissingInstance Text
  | UnknownTypeError Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ScopeErrorInfo
  = VariableNotInScope Text
  | UnknownScopeError Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

type TemplateName = Text

-- | Unified context for all stop hook templates
-- Flattened to avoid Maybe access issues in Ginger TH.
data StopHookContext = StopHookContext
  { template :: Text
  , stage :: Text
  , global_stops :: Int
  , stage_retries :: Int
  , build_failed :: Bool
  , errors :: [GHCError]
  , warnings :: [GHCWarning]
  , error_count :: Int
  , raw_output :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ToGVal Instances

type GingerRun = Run SourcePos (Writer Text) Text

instance ToGVal GingerRun StopHookContext where
  toGVal ctx = dict
    [ "template" ~> template ctx
    , "stage" ~> stage ctx
    , "global_stops" ~> global_stops ctx
    , "stage_retries" ~> stage_retries ctx
    , "build_failed" ~> build_failed ctx
    , "errors" ~> errors ctx
    , "warnings" ~> warnings ctx
    , "error_count" ~> error_count ctx
    , "raw_output" ~> raw_output ctx
    ]

instance ToGVal GingerRun GHCError where
  toGVal err = dict
    [ "file" ~> geFile err
    , "line" ~> geLine err
    , "column" ~> geColumn err
    , "message" ~> geMessage err
    , "error_type" ~> geErrorType err
    , "severity" ~> show (geSeverity err)
    ]

instance ToGVal GingerRun GHCErrorType where
  toGVal (TypeError info) = dict [ "tag" ~> ("TypeError" :: Text), "contents" ~> info ]
  toGVal (ScopeError info) = dict [ "tag" ~> ("ScopeError" :: Text), "contents" ~> info ]
  toGVal (ParseError msg) = dict [ "tag" ~> ("ParseError" :: Text), "contents" ~> msg ]
  toGVal (OtherError msg) = dict [ "tag" ~> ("OtherError" :: Text), "contents" ~> msg ]

instance ToGVal GingerRun TypeErrorInfo where
  toGVal (TypeMismatch exp' act) = dict
    [ "tag" ~> ("TypeMismatch" :: Text)
    , "teExpected" ~> exp'
    , "teActual" ~> act
    ]
  toGVal (AmbiguousType msg) = dict [ "tag" ~> ("AmbiguousType" :: Text), "contents" ~> msg ]
  toGVal (MissingInstance msg) = dict [ "tag" ~> ("MissingInstance" :: Text), "contents" ~> msg ]
  toGVal (UnknownTypeError msg) = dict [ "tag" ~> ("UnknownTypeError" :: Text), "contents" ~> msg ]

instance ToGVal GingerRun ScopeErrorInfo where
  toGVal (VariableNotInScope var) = dict
    [ "tag" ~> ("VariableNotInScope" :: Text)
    , "contents" ~> var
    ]
  toGVal (UnknownScopeError msg) = dict [ "tag" ~> ("UnknownScopeError" :: Text), "contents" ~> msg ]
