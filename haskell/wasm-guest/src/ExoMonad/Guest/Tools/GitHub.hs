{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module ExoMonad.Guest.Tools.GitHub
  ( GitHubListIssues,
    GitHubGetIssue,
    GitHubListPRs,
    GitHubListIssuesArgs (..),
    GitHubGetIssueArgs (..),
    GitHubListPRsArgs (..),
  )
where

import Data.Aeson (FromJSON, ToJSON, object)
import Data.Aeson qualified as Aeson
import ExoMonad.Guest.Tool.Class (MCPTool (..), successResult)
import GHC.Generics (Generic)

-- ListIssues
data GitHubListIssues

data GitHubListIssuesArgs = GitHubListIssuesArgs
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance MCPTool GitHubListIssues where
  type ToolArgs GitHubListIssues = GitHubListIssuesArgs
  toolName = "github_list_issues"
  toolDescription = "List GitHub issues"
  toolSchema = object []
  toolHandler _ = pure $ successResult $ Aeson.Array mempty -- Stub

-- GetIssue
data GitHubGetIssue

data GitHubGetIssueArgs = GitHubGetIssueArgs
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance MCPTool GitHubGetIssue where
  type ToolArgs GitHubGetIssue = GitHubGetIssueArgs
  toolName = "github_get_issue"
  toolDescription = "Get GitHub issue details"
  toolSchema = object []
  toolHandler _ = pure $ successResult $ Aeson.object [] -- Stub

-- ListPRs
data GitHubListPRs

data GitHubListPRsArgs = GitHubListPRsArgs
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance MCPTool GitHubListPRs where
  type ToolArgs GitHubListPRs = GitHubListPRsArgs
  toolName = "github_list_prs"
  toolDescription = "List GitHub PRs"
  toolSchema = object []
  toolHandler _ = pure $ successResult $ Aeson.Array mempty -- Stub
