{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Tools.GitHub
  ( -- * Tool Types
    GitHubListIssues,
    GitHubGetIssue,
    GitHubListPRs,
    GitHubListIssuesArgs (..),
    GitHubGetIssueArgs (..),
    GitHubListPRsArgs (..),

    -- * Tool Record
    GitHubTools (..),
    githubTools,
  )
where

import Data.Aeson (FromJSON (..), ToJSON, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import ExoMonad.Tool.Class (MCPTool (..), successResult)
import ExoMonad.Tool.Mode (ToolMode (..), AsHandler, mkHandler)
import GHC.Generics (Generic)

-- ============================================================================
-- ListIssues
-- ============================================================================
data GitHubListIssues

data GitHubListIssuesArgs = GitHubListIssuesArgs
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON GitHubListIssuesArgs where
  parseJSON = Aeson.withObject "GitHubListIssuesArgs" $ \_ -> pure GitHubListIssuesArgs

instance MCPTool GitHubListIssues where
  type ToolArgs GitHubListIssues = GitHubListIssuesArgs
  toolName = "github_list_issues"
  toolDescription = "List GitHub issues"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "properties" .= object []
      ]
  toolHandler _ = pure $ successResult $ Aeson.Array mempty -- Stub

-- ============================================================================
-- GetIssue
-- ============================================================================
data GitHubGetIssue

data GitHubGetIssueArgs = GitHubGetIssueArgs
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON GitHubGetIssueArgs where
  parseJSON = Aeson.withObject "GitHubGetIssueArgs" $ \_ -> pure GitHubGetIssueArgs

instance MCPTool GitHubGetIssue where
  type ToolArgs GitHubGetIssue = GitHubGetIssueArgs
  toolName = "github_get_issue"
  toolDescription = "Get GitHub issue details"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "properties" .= object []
      ]
  toolHandler _ = pure $ successResult $ Aeson.object [] -- Stub

-- ============================================================================
-- ListPRs
-- ============================================================================
data GitHubListPRs

data GitHubListPRsArgs = GitHubListPRsArgs
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON GitHubListPRsArgs where
  parseJSON = Aeson.withObject "GitHubListPRsArgs" $ \_ -> pure GitHubListPRsArgs

instance MCPTool GitHubListPRs where
  type ToolArgs GitHubListPRs = GitHubListPRsArgs
  toolName = "github_list_prs"
  toolDescription = "List GitHub PRs"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "properties" .= object []
      ]
  toolHandler _ = pure $ successResult $ Aeson.Array mempty -- Stub

-- ============================================================================
-- Tool Record
-- ============================================================================

data GitHubTools mode = GitHubTools
  { listIssues :: mode :- GitHubListIssues,
    getIssue :: mode :- GitHubGetIssue,
    listPRs :: mode :- GitHubListPRs
  }
  deriving (Generic)

-- | Pre-built value with handlers
githubTools :: GitHubTools AsHandler
githubTools =
  GitHubTools
    { listIssues = mkHandler @GitHubListIssues,
      getIssue = mkHandler @GitHubGetIssue,
      listPRs = mkHandler @GitHubListPRs
    }
