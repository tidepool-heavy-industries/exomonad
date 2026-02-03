{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module ExoMonad.Guest.Tools.Git
  ( GitBranch,
    GitStatus,
    GitLog,
    GitBranchArgs (..),
    GitStatusArgs (..),
    GitLogArgs (..),
  )
where

import Data.Aeson (FromJSON, ToJSON, object)
import Data.Aeson qualified as Aeson
import ExoMonad.Guest.Tool.Class (MCPTool (..), successResult)
import GHC.Generics (Generic)

-- GitBranch
data GitBranch

data GitBranchArgs = GitBranchArgs
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance MCPTool GitBranch where
  type ToolArgs GitBranch = GitBranchArgs
  toolName = "git_branch"
  toolDescription = "Get current git branch"
  toolSchema = object []
  toolHandler _ = pure $ successResult $ Aeson.String "main" -- Stub

-- GitStatus
data GitStatus

data GitStatusArgs = GitStatusArgs
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance MCPTool GitStatus where
  type ToolArgs GitStatus = GitStatusArgs
  toolName = "git_status"
  toolDescription = "Get git status"
  toolSchema = object []
  toolHandler _ = pure $ successResult $ Aeson.String "clean" -- Stub

-- GitLog
data GitLog

data GitLogArgs = GitLogArgs
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance MCPTool GitLog where
  type ToolArgs GitLog = GitLogArgs
  toolName = "git_log"
  toolDescription = "Get git log"
  toolSchema = object []
  toolHandler _ = pure $ successResult $ Aeson.String "commit 123" -- Stub
