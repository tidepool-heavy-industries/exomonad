{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Tools.Git
  ( -- * Tool Types
    GitBranch,
    GitStatus,
    GitLog,
    
    -- * Tool Record
    GitTools (..),
    gitTools,
  )
where

import Data.Aeson (FromJSON (..), ToJSON, object, (.=), (.:?))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import ExoMonad.Tool.Class (MCPTool (..), successResult, errorResult)
import ExoMonad.Tool.Mode (ToolMode (..), AsHandler, mkHandler)
import ExoMonad.HostCall (callHost, host_git_get_branch, host_git_get_dirty_files, host_git_get_recent_commits)
import ExoMonad.FFI (FFIBoundary, GitHostInput (..), GitLogInput (..), Commit (..))
import GHC.Generics (Generic)
import Prelude hiding (log)

-- ============================================================================
-- GitBranch
-- ============================================================================
data GitBranch

data GitBranchArgs = GitBranchArgs
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON GitBranchArgs where
  parseJSON = Aeson.withObject "GitBranchArgs" $ \_ -> pure GitBranchArgs

instance FFIBoundary GitBranchArgs

instance MCPTool GitBranch where
  type ToolArgs GitBranch = GitBranchArgs
  toolName = "git_branch"
  toolDescription = "Get current git branch"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "properties" .= object []
      ]
  toolHandler _ = do
    let input = GitHostInput { workingDir = ".", containerId = "local" }
    res <- callHost @GitHostInput @Text host_git_get_branch input
    case res of
      Left err -> pure $ errorResult err
      Right branch -> pure $ successResult $ Aeson.String branch

-- ============================================================================
-- GitStatus
-- ============================================================================
data GitStatus

data GitStatusArgs = GitStatusArgs
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON GitStatusArgs where
  parseJSON = Aeson.withObject "GitStatusArgs" $ \_ -> pure GitStatusArgs

instance FFIBoundary GitStatusArgs

instance MCPTool GitStatus where
  type ToolArgs GitStatus = GitStatusArgs
  toolName = "git_status"
  toolDescription = "Get git status (dirty files)"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "properties" .= object []
      ]
  toolHandler _ = do
    let input = GitHostInput { workingDir = ".", containerId = "local" }
    res <- callHost @GitHostInput @[Text] host_git_get_dirty_files input
    case res of
      Left err -> pure $ errorResult err
      Right files -> pure $ successResult $ Aeson.toJSON files

-- ============================================================================
-- GitLog
-- ============================================================================
data GitLog

data GitLogArgs = GitLogArgs
  { limit :: Maybe Int }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON GitLogArgs where
  parseJSON = Aeson.withObject "GitLogArgs" $ \v ->
    GitLogArgs <$> v .:? "limit"

instance FFIBoundary GitLogArgs

instance MCPTool GitLog where
  type ToolArgs GitLog = GitLogArgs
  toolName = "git_log"
  toolDescription = "Get git log (recent commits)"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "properties" .= object
          [ "limit" .= object
            [ "type" .= ("integer" :: Text)
            , "description" .= ("Number of commits to return (default: 10)" :: Text)
            ]
          ]
      ]
  toolHandler args = do
    let input = GitLogInput
          { gliWorkingDir = "."
          , gliContainerId = "local"
          , gliLimit = fromMaybe 10 (limit args)
          }
    res <- callHost @GitLogInput @[Commit] host_git_get_recent_commits input
    case res of
      Left err -> pure $ errorResult err
      Right commits -> pure $ successResult $ Aeson.toJSON commits

-- ============================================================================
-- Tool Record
-- ============================================================================

data GitTools mode = GitTools
  { branch :: mode :- GitBranch,
    status :: mode :- GitStatus,
    log :: mode :- GitLog
  }
  deriving (Generic)

-- | Pre-built value with handlers
gitTools :: GitTools AsHandler
gitTools =
  GitTools
    { branch = mkHandler @GitBranch,
      status = mkHandler @GitStatus,
      log = mkHandler @GitLog
    }
