{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ExoMonad.Control.TLTools.Types
  ( -- * Enums
    IssueCategory(..)
  , Priority(..)
  , Severity(..)
  , Component(..)
    -- * Nested Types
  , Classification(..)
  , IssueContent(..)
  , Criterion(..)
  , Assignment(..)
    -- * Tool Args/Result
  , TLCreateIssueArgs(..)
  , TLCreateIssueResult(..)
  ) where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Text (Text)
import GHC.Generics (Generic)
import ExoMonad.Schema (deriveMCPTypeWith, deriveMCPEnum, defaultMCPOptions, (??), MCPOptions(..))

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
  { clCategory   :: IssueCategory
  , clPriority   :: Priority
  , clSeverity   :: Maybe Severity
  , clComponents :: [Component]
  }
  deriving stock (Show, Eq, Generic)

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "cl" } ''Classification
  [ 'clCategory   ?? "Issue category"
  , 'clPriority   ?? "Priority level"
  , 'clSeverity   ?? "Severity (recommended for bugs)"
  , 'clComponents ?? "Affected system components"
  ])

-- | Single acceptance criterion.
data Criterion = Criterion
  { crItem     :: Text
  , crTestable :: Bool
  }
  deriving stock (Show, Eq, Generic)

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "cr" } ''Criterion
  [ 'crItem     ?? "Acceptance criterion description"
  , 'crTestable ?? "Can this be verified programmatically?"
  ])

-- | Issue content: description, reproduction steps, acceptance criteria.
data IssueContent = IssueContent
  { icDescription        :: Text
  , icReproduction       :: Maybe Text
  , icAcceptanceCriteria :: [Criterion]
  }
  deriving stock (Show, Eq, Generic)

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "ic" } ''IssueContent
  [ 'icDescription        ?? "Detailed description of the issue"
  , 'icReproduction       ?? "Steps to reproduce (for bugs)"
  , 'icAcceptanceCriteria ?? "Definition of done checklist"
  ])

-- | Issue assignment: assignees and additional labels.
data Assignment = Assignment
  { asAssignees :: [Text]
  , asLabels    :: Maybe [Text]
  }
  deriving stock (Show, Eq, Generic)

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "as" } ''Assignment
  [ 'asAssignees ?? "GitHub usernames to assign"
  , 'asLabels    ?? "Additional labels beyond auto-generated"
  ])

-- ══════════════════════════════════════════════════════════════════════════════
-- TOOL ARGS/RESULT
-- ══════════════════════════════════════════════════════════════════════════════

-- | Arguments for tl_create_issue tool.
--
-- Structured interface for creating issues with nested classification,
-- content, and assignment sections.
data TLCreateIssueArgs = TLCreateIssueArgs
  { tliaTitle          :: Text
  , tliaClassification :: Classification
  , tliaContent        :: IssueContent
  , tliaAssignment     :: Maybe Assignment
  }
  deriving stock (Show, Eq, Generic)

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "tlia" } ''TLCreateIssueArgs
  [ 'tliaTitle          ?? "Concise issue title"
  , 'tliaClassification ?? "Category, priority, severity, components"
  , 'tliaContent        ?? "Description, reproduction, acceptance criteria"
  , 'tliaAssignment     ?? "Assignees and additional labels"
  ])

-- | Result of tl_create_issue tool.
data TLCreateIssueResult = TLCreateIssueResult
  { tlcrNumber  :: Int
  , tlcrUrl     :: Text
  , tlcrSuccess :: Bool
  , tlcrError   :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON TLCreateIssueResult where
  toJSON r = object
    [ "number"  .= tlcrNumber r
    , "url"     .= tlcrUrl r
    , "success" .= tlcrSuccess r
    , "error"   .= tlcrError r
    ]
