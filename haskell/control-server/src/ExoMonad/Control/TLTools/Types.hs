{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ExoMonad.Control.TLTools.Types
  ( -- * Enums
    IssueCategory (..),
    Priority (..),
    Severity (..),
    Component (..),

    -- * Nested Types
    Classification (..),
    IssueContent (..),
    Criterion (..),
    Assignment (..),

    -- * Tool Args/Result
    TLCreateIssueArgs (..),
    TLCreateIssueResult (..),
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import ExoMonad.Schema (defaultMCPOptions, deriveMCPEnum, deriveMCPTypeWith, (??))
import GHC.Generics (Generic)
import Language.Haskell.TH (mkName)

-- ══════════════════════════════════════════════════════════════════════════════
-- ENUMS
-- ══════════════════════════════════════════════════════════════════════════════

-- | Issue category for classification.
data IssueCategory = Bug | Feature | Refactor | Docs | Experiment
  deriving stock (Show, Eq, Generic, Enum, Bounded)

$(deriveMCPEnum ''IssueCategory)

-- | Issue priority level.
data Priority = P0 | P1 | P2 | P3
  deriving stock (Show, Eq, Generic, Enum, Bounded)

$(deriveMCPEnum ''Priority)

-- | Issue severity (mainly for bugs).
data Severity = Critical | High | Medium | Low
  deriving stock (Show, Eq, Generic, Enum, Bounded)

$(deriveMCPEnum ''Severity)

-- | System component affected by the issue.
data Component
  = ControlServer
  | DSL
  | TUI
  | Docker
  | Hooks
  | MCP
  | Zellij
  | Other
  deriving stock (Show, Eq, Generic, Enum, Bounded)

$(deriveMCPEnum ''Component)

-- ══════════════════════════════════════════════════════════════════════════════
-- NESTED TYPES
-- ══════════════════════════════════════════════════════════════════════════════

-- | Issue classification metadata.
data Classification = Classification
  { category :: IssueCategory,
    priority :: Priority,
    severity :: Maybe Severity,
    components :: [Component]
  }
  deriving stock (Show, Eq, Generic)

$( deriveMCPTypeWith
     defaultMCPOptions
     ''Classification
     [ mkName "category" ?? "Issue category",
       mkName "priority" ?? "Priority level",
       mkName "severity" ?? "Severity (recommended for bugs)",
       mkName "components" ?? "Affected system components"
     ]
 )

-- | Single acceptance criterion.
data Criterion = Criterion
  { item :: Text,
    testable :: Bool
  }
  deriving stock (Show, Eq, Generic)

$( deriveMCPTypeWith
     defaultMCPOptions
     ''Criterion
     [ mkName "item" ?? "Acceptance criterion description",
       mkName "testable" ?? "Can this be verified programmatically?"
     ]
 )

-- | Issue content: description, reproduction steps, acceptance criteria.
data IssueContent = IssueContent
  { description :: Text,
    reproduction :: Maybe Text,
    acceptanceCriteria :: [Criterion]
  }
  deriving stock (Show, Eq, Generic)

$( deriveMCPTypeWith
     defaultMCPOptions
     ''IssueContent
     [ mkName "description" ?? "Detailed description of the issue",
       mkName "reproduction" ?? "Steps to reproduce (for bugs)",
       mkName "acceptanceCriteria" ?? "Definition of done checklist"
     ]
 )

-- | Issue assignment: assignees and additional labels.
data Assignment = Assignment
  { assignees :: [Text],
    labels :: Maybe [Text]
  }
  deriving stock (Show, Eq, Generic)

$( deriveMCPTypeWith
     defaultMCPOptions
     ''Assignment
     [ mkName "assignees" ?? "GitHub usernames to assign",
       mkName "labels" ?? "Additional labels beyond auto-generated"
     ]
 )

-- ══════════════════════════════════════════════════════════════════════════════
-- TOOL ARGS/RESULT
-- ══════════════════════════════════════════════════════════════════════════════

-- | Arguments for tl_create_issue tool.
--
-- Structured interface for creating issues with nested classification,
-- content, and assignment sections.
data TLCreateIssueArgs = TLCreateIssueArgs
  { title :: Text,
    classification :: Classification,
    content :: IssueContent,
    assignment :: Maybe Assignment
  }
  deriving stock (Show, Eq, Generic)

$( deriveMCPTypeWith
     defaultMCPOptions
     ''TLCreateIssueArgs
     [ mkName "title" ?? "Concise issue title",
       mkName "classification" ?? "Category, priority, severity, components",
       mkName "content" ?? "Description, reproduction, acceptance criteria",
       mkName "assignment" ?? "Assignees and additional labels"
     ]
 )

-- | Result of tl_create_issue tool.
data TLCreateIssueResult = TLCreateIssueResult
  { number :: Int,
    url :: Text,
    success :: Bool,
    error :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON TLCreateIssueResult where
  toJSON r =
    object
      [ "number" .= r.number,
        "url" .= r.url,
        "success" .= r.success,
        "error" .= r.error
      ]
