{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Template context types for SessionStart hook.
--
-- This module is separate from SessionStart.hs due to TH staging requirements.
-- The context types and ToGVal instances must be compiled before the
-- TH splices that reference them.
module Tidepool.Control.Hook.SessionStart.Context
  ( SessionStartContext(..)
  , BeadContext(..)
  , DepContext(..)
  ) where

import Control.Monad.Writer (Writer)
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Ginger.GVal (ToGVal(..), dict, (~>))
import Text.Ginger.Run.Type (Run)
import Text.Parsec.Pos (SourcePos)


-- | Context for the session start prompt.
--
-- Contains all information needed to render the @session-start.jinja@ template.
data SessionStartContext = SessionStartContext
  { bead_id :: Maybe Text
    -- ^ Bead ID (e.g., "tidepool-51i") if on a bd-* branch
  , branch :: Maybe Text
    -- ^ Current branch name
  , cwd :: Text
    -- ^ Current working directory
  , bead :: Maybe BeadContext
    -- ^ Full bead details if available
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
    [ "bead_id" ~> bead_id ctx
    , "branch" ~> branch ctx
    , "cwd" ~> cwd ctx
    , "bead" ~> bead ctx
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
    , "depends_on" ~> bc.depends_on
    , "blocks" ~> bc.blocks
    ]

-- | ToGVal instance for dependency context.
instance ToGVal (Run SourcePos (Writer Text) Text) DepContext where
  toGVal dc = dict
    [ "id" ~> dc.dep_id
    , "title" ~> dc.dep_title
    , "priority" ~> dc.dep_priority
    ]
