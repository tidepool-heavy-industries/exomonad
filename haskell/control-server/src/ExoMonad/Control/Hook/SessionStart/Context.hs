{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Template context types for SessionStart hook.
--
-- This module is separate from SessionStart.hs due to TH staging requirements.
-- The context types and ToGVal instances must be compiled before the
-- TH splices that reference them.
module ExoMonad.Control.Hook.SessionStart.Context
  ( SessionStartContext(..)
  , IssuesDashboardContext(..)
  , PMDashboardContext(..)
  , IssueContext(..)
  ) where

import Control.Monad.Writer (Writer)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Text.Ginger.GVal (ToGVal(..), dict, (~>))
import Text.Ginger.Run.Type (Run)
import Text.Parsec.Pos (SourcePos)

import ExoMonad.Control.RoleConfig (Role)


-- | Context for the session start prompt.
--
-- Contains all information needed to render the @session-start.jinja@ template.
data SessionStartContext = SessionStartContext
  { role :: Role
    -- ^ Role of the agent (Dev, TL, PM)
  , issue_number :: Maybe Int
    -- ^ Issue number (e.g., 123) if on a gh-* branch
  , branch :: Maybe Text
    -- ^ Current branch name
  , cwd :: Text
    -- ^ Current working directory
  , issue :: Maybe IssueContext
    -- ^ Full issue details if available
  , dashboard :: Maybe IssuesDashboardContext
    -- ^ Issues dashboard (for TL role)
  , pm_dashboard :: Maybe PMDashboardContext
    -- ^ PM dashboard (for PM role)
  } deriving stock (Show, Eq, Generic)

-- | Issues dashboard context for TL role.
data IssuesDashboardContext = IssuesDashboardContext
  { open :: [IssueContext]
  } deriving stock (Show, Eq, Generic)

-- | PM dashboard context.
data PMDashboardContext = PMDashboardContext
  { inboxCount :: Int
  , urgentMessages :: [Text]
  , activeConvoys :: [Text]
  , agentStatus :: Text
  , pendingPRs :: [Text]
  } deriving stock (Show, Eq, Generic)

-- | Issue context for template rendering.
-- Field names match template variable names for ginger TH validation.
data IssueContext = IssueContext
  { number :: Int
  , title :: Text
  , status :: Text
  , author :: Text
  , labels :: [Text]
  , description :: Text
  , url :: Text
  } deriving stock (Show, Eq, Generic)


-- | ToGVal instance for template rendering.
instance ToGVal (Run SourcePos (Writer Text) Text) SessionStartContext where
  toGVal ctx = dict
    [ "role" ~> (T.toLower . T.pack . show $ ctx.role)
    , "issue_number" ~> ctx.issue_number
    , "branch" ~> ctx.branch
    , "cwd" ~> ctx.cwd
    , "issue" ~> ctx.issue
    , "dashboard" ~> ctx.dashboard
    , "pm_dashboard" ~> ctx.pm_dashboard
    ]

-- | ToGVal instance for issues dashboard context.
instance ToGVal (Run SourcePos (Writer Text) Text) IssuesDashboardContext where
  toGVal db = dict
    [ "open" ~> db.open
    ]

-- | ToGVal instance for PM dashboard context.
instance ToGVal (Run SourcePos (Writer Text) Text) PMDashboardContext where
  toGVal db = dict
    [ "inboxCount" ~> db.inboxCount
    , "urgentMessages" ~> db.urgentMessages
    , "activeConvoys" ~> db.activeConvoys
    , "agentStatus" ~> db.agentStatus
    , "pendingPRs" ~> db.pendingPRs
    ]

-- | ToGVal instance for issue context.
instance ToGVal (Run SourcePos (Writer Text) Text) IssueContext where
  toGVal ic = dict
    [ "number" ~> ic.number
    , "title" ~> ic.title
    , "status" ~> ic.status
    , "author" ~> ic.author
    , "labels" ~> ic.labels
    , "description" ~> ic.description
    , "url" ~> ic.url
    ]
