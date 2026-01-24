{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Template context types for SessionStart hook.
--
-- This module is separate from SessionStart.hs due to TH staging requirements.
-- The context types and ToGVal instances must be compiled before the
-- TH splices that reference them.
module Tidepool.Control.Hook.SessionStart.Context
  ( SessionStartContext(..)
  , BeadsDashboardContext(..)
  , BeadContext(..)
  , DepContext(..)
  ) where

import Control.Monad.Writer (Writer)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Text.Ginger.GVal (ToGVal(..), dict, (~>))
import Text.Ginger.Run.Type (Run)
import Text.Parsec.Pos (SourcePos)

import Tidepool.Control.RoleConfig (Role)


-- | Context for the session start prompt.
--
-- Contains all information needed to render the @session-start.jinja@ template.
data SessionStartContext = SessionStartContext
  { role :: Role
    -- ^ Role of the agent (Dev, TL, PM)
  , bead_id :: Maybe Text
    -- ^ Bead ID (e.g., "tidepool-51i") if on a bd-* branch
  , branch :: Maybe Text
    -- ^ Current branch name
  , cwd :: Text
    -- ^ Current working directory
  , bead :: Maybe BeadContext
    -- ^ Full bead details if available
  , dashboard :: Maybe BeadsDashboardContext
    -- ^ Beads dashboard (for TL role)
  } deriving stock (Show, Eq, Generic)

-- | Beads dashboard context for TL role.
data BeadsDashboardContext = BeadsDashboardContext
  { ready :: [BeadContext]
  , in_progress :: [BeadContext]
  , blocked :: [BeadContext]
  } deriving stock (Show, Eq, Generic)

-- | Bead context for template rendering.
-- Field names match template variable names for ginger TH validation.
data BeadContext = BeadContext
  { id :: Text
  , title :: Text
  , priority :: Text
  , status :: Text
  , owner :: Maybe Text
  , type_ :: Text
  , created :: Text
  , updated :: Text
  , description :: Text
  , acceptance_criteria :: Maybe Text
  , depends_on :: [DepContext]
  , blocks :: [DepContext]
  } deriving stock (Show, Eq, Generic)

-- | Dependency context for template rendering.
-- Field names match template variable names for ginger TH validation.
data DepContext = DepContext
  { dep_id :: Text
  , dep_title :: Text
  , dep_priority :: Text
  } deriving stock (Show, Eq, Generic)


-- | ToGVal instance for template rendering.
instance ToGVal (Run SourcePos (Writer Text) Text) SessionStartContext where
  toGVal ctx = dict
    [ "role" ~> (T.toLower . T.pack . show $ ctx.role)
    , "bead_id" ~> bead_id ctx
    , "branch" ~> branch ctx
    , "cwd" ~> cwd ctx
    , "bead" ~> bead ctx
    , "dashboard" ~> dashboard ctx
    ]

-- | ToGVal instance for beads dashboard context.
instance ToGVal (Run SourcePos (Writer Text) Text) BeadsDashboardContext where
  toGVal db = dict
    [ "ready" ~> db.ready
    , "in_progress" ~> db.in_progress
    , "blocked" ~> db.blocked
    ]

-- | ToGVal instance for bead context.
instance ToGVal (Run SourcePos (Writer Text) Text) BeadContext where
  toGVal bc = dict
    [ "id" ~> bc.id
    , "title" ~> bc.title
    , "priority" ~> bc.priority
    , "status" ~> bc.status
    , "owner" ~> bc.owner
    , "type_" ~> bc.type_
    , "created" ~> bc.created
    , "updated" ~> bc.updated
    , "description" ~> bc.description
    , "acceptance_criteria" ~> bc.acceptance_criteria
    , "depends_on" ~> bc.depends_on
    , "blocks" ~> bc.blocks
    ]

-- | ToGVal instance for dependency context.
-- Keys must match Haskell field names for TH validation.
instance ToGVal (Run SourcePos (Writer Text) Text) DepContext where
  toGVal dc = dict
    [ "dep_id" ~> dc.dep_id
    , "dep_title" ~> dc.dep_title
    , "dep_priority" ~> dc.dep_priority
    ]
