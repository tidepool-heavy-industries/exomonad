{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module ExoMonad.Control.GHTools.Types
  ( -- * List Tool
    GHIssueListArgs (..),
    GHIssueListResult (..),
    IssueSummary (..),

    -- * Show Tool
    GHIssueShowArgs (..),
    GHIssueShowResult (..),

    -- * Create Tool
    GHIssueCreateArgs (..),
    GHIssueCreateResult (..),

    -- * Update Tool
    GHIssueUpdateArgs (..),
    GHIssueUpdateResult (..),

    -- * Close Tool
    GHIssueCloseArgs (..),
    GHIssueCloseResult (..),

    -- * Reopen Tool
    GHIssueReopenArgs (..),
    GHIssueReopenResult (..),
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import ExoMonad.Effects.GitHub (Issue)
import ExoMonad.Schema (defaultMCPOptions, deriveMCPTypeWith, (??))
import GHC.Generics (Generic)
import Language.Haskell.TH (mkName)

-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE LIST TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for gh_issue_list tool.
data GHIssueListArgs = GHIssueListArgs
  { -- | owner/repo
    repo :: Maybe Text,
    -- | open, closed
    status :: Maybe Text,
    -- | Filter by labels
    labels :: Maybe [Text],
    -- | Max results
    limit :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)

$( deriveMCPTypeWith
     defaultMCPOptions
     ''GHIssueListArgs
     [ mkName "repo" ?? "Repository in owner/repo format (optional, uses environment default if omitted)",
       mkName "status" ?? "Filter by status: open, closed",
       mkName "labels" ?? "Filter by labels",
       mkName "limit" ?? "Max results"
     ]
 )

-- | Summary of an issue for list responses (no body/comments).
data IssueSummary = IssueSummary
  { number :: Int,
    title :: Text,
    state :: Text, -- "OPEN" or "CLOSED"
    labels :: [Text],
    url :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON IssueSummary where
  toJSON s =
    object
      [ "number" .= s.number,
        "title" .= s.title,
        "state" .= s.state,
        "labels" .= s.labels,
        "url" .= s.url
      ]

-- | Result of gh_issue_list tool.
data GHIssueListResult = GHIssueListResult
  { issues :: [IssueSummary],
    count :: Int,
    error :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON GHIssueListResult where
  toJSON res =
    object
      [ "issues" .= res.issues,
        "count" .= res.count,
        "error" .= res.error
      ]

-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE SHOW TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for gh_issue_show tool.
data GHIssueShowArgs = GHIssueShowArgs
  { repo :: Maybe Text,
    number :: Int
  }
  deriving stock (Show, Eq, Generic)

$( deriveMCPTypeWith
     defaultMCPOptions
     ''GHIssueShowArgs
     [ mkName "repo" ?? "Repository in owner/repo format",
       mkName "number" ?? "The issue number to show"
     ]
 )

-- | Result of gh_issue_show tool.
data GHIssueShowResult = GHIssueShowResult
  { issue :: Maybe Issue,
    found :: Bool,
    error :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON GHIssueShowResult where
  toJSON res =
    object
      [ "issue" .= res.issue,
        "found" .= res.found,
        "error" .= res.error
      ]

-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE CREATE TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for gh_issue_create tool.
data GHIssueCreateArgs = GHIssueCreateArgs
  { repo :: Maybe Text,
    title :: Text,
    body :: Maybe Text,
    labels :: Maybe [Text],
    assignees :: Maybe [Text]
  }
  deriving stock (Show, Eq, Generic)

$( deriveMCPTypeWith
     defaultMCPOptions
     ''GHIssueCreateArgs
     [ mkName "repo" ?? "Repository in owner/repo format",
       mkName "title" ?? "Title of the new issue",
       mkName "body" ?? "Body description of the issue",
       mkName "labels" ?? "Labels to attach",
       mkName "assignees" ?? "Assignee usernames"
     ]
 )

-- | Result of gh_issue_create tool.
data GHIssueCreateResult = GHIssueCreateResult
  { number :: Int,
    success :: Bool,
    error :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON GHIssueCreateResult where
  toJSON res =
    object
      [ "number" .= res.number,
        "success" .= res.success,
        "error" .= res.error
      ]

-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE UPDATE TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for gh_issue_update tool.
data GHIssueUpdateArgs = GHIssueUpdateArgs
  { repo :: Maybe Text,
    number :: Int,
    title :: Maybe Text,
    body :: Maybe Text,
    -- | open, closed
    status :: Maybe Text,
    labels :: Maybe [Text],
    assignees :: Maybe [Text]
  }
  deriving stock (Show, Eq, Generic)

$( deriveMCPTypeWith
     defaultMCPOptions
     ''GHIssueUpdateArgs
     [ mkName "repo" ?? "Repository in owner/repo format",
       mkName "number" ?? "The issue number to update",
       mkName "title" ?? "New title",
       mkName "body" ?? "New body description",
       mkName "status" ?? "New status: open, closed",
       mkName "labels" ?? "New set of labels (replaces existing)",
       mkName "assignees" ?? "New set of assignees (replaces existing)"
     ]
 )

-- | Result of gh_issue_update tool.
data GHIssueUpdateResult = GHIssueUpdateResult
  { success :: Bool,
    number :: Int,
    error :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON GHIssueUpdateResult where
  toJSON res =
    object
      [ "success" .= res.success,
        "number" .= res.number,
        "error" .= res.error
      ]

-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE CLOSE TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for gh_issue_close tool.
data GHIssueCloseArgs = GHIssueCloseArgs
  { repo :: Maybe Text,
    number :: Int
  }
  deriving stock (Show, Eq, Generic)

$( deriveMCPTypeWith
     defaultMCPOptions
     ''GHIssueCloseArgs
     [ mkName "repo" ?? "Repository in owner/repo format",
       mkName "number" ?? "The issue number to close"
     ]
 )

-- | Result of gh_issue_close tool.
data GHIssueCloseResult = GHIssueCloseResult
  { success :: Bool,
    number :: Int,
    error :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON GHIssueCloseResult where
  toJSON res =
    object
      [ "success" .= res.success,
        "number" .= res.number,
        "error" .= res.error
      ]

-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE REOPEN TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for gh_issue_reopen tool.
data GHIssueReopenArgs = GHIssueReopenArgs
  { repo :: Maybe Text,
    number :: Int
  }
  deriving stock (Show, Eq, Generic)

$( deriveMCPTypeWith
     defaultMCPOptions
     ''GHIssueReopenArgs
     [ mkName "repo" ?? "Repository in owner/repo format",
       mkName "number" ?? "The issue number to reopen"
     ]
 )

-- | Result of gh_issue_reopen tool.
data GHIssueReopenResult = GHIssueReopenResult
  { success :: Bool,
    number :: Int,
    error :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON GHIssueReopenResult where
  toJSON res =
    object
      [ "success" .= res.success,
        "number" .= res.number,
        "error" .= res.error
      ]
