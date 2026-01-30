{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Template context types for Stop hook.
--
-- This module is separate from the Stop hook handler due to TH staging requirements.
-- The context types and ToGVal instances must be compiled before the
-- TH splices that reference them.
module ExoMonad.Control.Hook.Stop.Context
  ( StopContext (..),
    StopPRContext (..),
    StopPreCommitContext (..),
  )
where

import Control.Monad.Writer (Writer)
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Ginger.GVal (ToGVal (..), dict, (~>))
import Text.Ginger.Run.Type (Run)
import Text.Parsec.Pos (SourcePos)

-- | Context for the Stop hook template.
data StopContext = StopContext
  { -- | Issue number if on a gh-* branch
    issue_number :: Maybe Int,
    -- | Current branch name
    branch :: Text,
    -- | Files with uncommitted changes
    dirty_files :: [Text],
    -- | Number of commits ahead of origin/main
    commits_ahead :: Int,
    -- | PR info if one exists for this branch
    pr :: Maybe StopPRContext,
    -- | True if no action needed (no dirty files, has PR or not on issue branch)
    clean :: Bool,
    -- | Pre-commit check results (if run)
    pre_commit :: Maybe StopPreCommitContext,
    -- | True if issue was closed as part of this stop
    issue_closed :: Bool,
    -- | True if issue was already closed before stop
    issue_already_closed :: Bool
  }
  deriving stock (Show, Eq, Generic)

-- | Pre-commit check context for Stop hook.
data StopPreCommitContext = StopPreCommitContext
  { -- | Whether pre-commit checks passed
    success :: Bool,
    -- | Combined stdout/stderr from checks
    output :: Text
  }
  deriving stock (Show, Eq, Generic)

-- | PR context for Stop hook.
data StopPRContext = StopPRContext
  { number :: Int,
    url :: Text,
    -- | Number of unresolved review comments
    pending_comments :: Int
  }
  deriving stock (Show, Eq, Generic)

instance ToGVal (Run SourcePos (Writer Text) Text) StopContext where
  toGVal ctx =
    dict
      [ "issue_number" ~> ctx.issue_number,
        "branch" ~> ctx.branch,
        "dirty_files" ~> ctx.dirty_files,
        "commits_ahead" ~> ctx.commits_ahead,
        "pr" ~> ctx.pr,
        "clean" ~> ctx.clean,
        "pre_commit" ~> ctx.pre_commit,
        "issue_closed" ~> ctx.issue_closed,
        "issue_already_closed" ~> ctx.issue_already_closed
      ]

instance ToGVal (Run SourcePos (Writer Text) Text) StopPRContext where
  toGVal p =
    dict
      [ "number" ~> p.number,
        "url" ~> p.url,
        "pending_comments" ~> p.pending_comments
      ]

instance ToGVal (Run SourcePos (Writer Text) Text) StopPreCommitContext where
  toGVal p =
    dict
      [ "success" ~> p.success,
        "output" ~> p.output
      ]
