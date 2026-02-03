{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

-- | GitHub tool records.
module ExoMonad.Guest.Records.GitHub
  ( GitHubTools (..),
    githubToolsHandler,
    githubToolsSchema,
    githubTools, -- Convenience alias for handler
  )
where

import ExoMonad.Guest.Tool.Mode (AsHandler, AsSchema, ToolMode ((:-)), mkHandler, mkSchema)
import ExoMonad.Guest.Tools.GitHub (GitHubGetIssue, GitHubListIssues, GitHubListPRs)
import GHC.Generics (Generic)

data GitHubTools mode = GitHubTools
  { listIssues :: mode :- GitHubListIssues,
    getIssue :: mode :- GitHubGetIssue,
    listPRs :: mode :- GitHubListPRs
  }
  deriving (Generic)

githubToolsHandler :: GitHubTools AsHandler
githubToolsHandler =
  GitHubTools
    { listIssues = mkHandler @GitHubListIssues,
      getIssue = mkHandler @GitHubGetIssue,
      listPRs = mkHandler @GitHubListPRs
    }

githubToolsSchema :: GitHubTools AsSchema
githubToolsSchema =
  GitHubTools
    { listIssues = mkSchema @GitHubListIssues,
      getIssue = mkSchema @GitHubGetIssue,
      listPRs = mkSchema @GitHubListPRs
    }

-- | Default handler instance for use in Role.hs
githubTools :: GitHubTools AsHandler
githubTools = githubToolsHandler
