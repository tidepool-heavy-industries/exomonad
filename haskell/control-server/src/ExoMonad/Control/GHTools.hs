{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | GitHub MCP tools for centralized issue operations.
module ExoMonad.Control.GHTools
  ( -- * List Tool
    ghIssueListLogic
  , GHIssueListArgs(..)
  , GHIssueListResult(..)

    -- * Show Tool
  , ghIssueShowLogic
  , GHIssueShowArgs(..)
  , GHIssueShowResult(..)

    -- * Create Tool
  , ghIssueCreateLogic
  , GHIssueCreateArgs(..)
  , GHIssueCreateResult(..)

    -- * Update Tool
  , ghIssueUpdateLogic
  , GHIssueUpdateArgs(..)
  , GHIssueUpdateResult(..)

    -- * Close Tool
  , ghIssueCloseLogic
  , GHIssueCloseArgs(..)
  , GHIssueCloseResult(..)

    -- * Reopen Tool
  , ghIssueReopenLogic
  , GHIssueReopenArgs(..)
  , GHIssueReopenResult(..)

    -- * Helpers
  , getRepo
  ) where

import Control.Monad.Freer (Eff, Member)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import ExoMonad.Effects.GitHub
  ( GitHub, IssueState(..), IssueFilter(..)
  , CreateIssueInput(..), UpdateIssueInput(..), defaultIssueFilter, emptyUpdateIssueInput
  , listIssues, getIssue, createIssue, updateIssue, closeIssue, reopenIssue
  )
import ExoMonad.Effects.Env (Env)

import ExoMonad.Control.GHTools.Types
import ExoMonad.Control.Combinators (withGitHubRepo, getRepo)

-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE LIST TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Core logic for gh_issue_list.
ghIssueListLogic
  :: (Member GitHub es, Member Env es)
  => GHIssueListArgs
  -> Eff es GHIssueListResult
ghIssueListLogic args =
  withGitHubRepo args.repo
    (\repo -> listIssues repo (defaultIssueFilter
        { ifLabels = fromMaybe [] args.labels
        , ifState  = parseIssueState =<< args.status
        , ifLimit  = args.limit
        }))
    (\is -> GHIssueListResult
      { issues = is
      , count  = length is
      , error  = Nothing
      })
    (\err -> GHIssueListResult
      { issues = []
      , count  = 0
      , error  = Just err
      })


-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE SHOW TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Core logic for gh_issue_show.
ghIssueShowLogic
  :: (Member GitHub es, Member Env es)
  => GHIssueShowArgs
  -> Eff es GHIssueShowResult
ghIssueShowLogic args =
  withGitHubRepo args.repo
    (\repo -> getIssue repo args.number True) -- Include comments
    (\maybeIssue -> GHIssueShowResult
      { issue = maybeIssue
      , found = case maybeIssue of
          Just _ -> True
          Nothing -> False
      , error = Nothing
      })
    (\err -> GHIssueShowResult
      { issue = Nothing
      , found = False
      , error = Just err
      })


-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE CREATE TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Core logic for gh_issue_create.
ghIssueCreateLogic
  :: (Member GitHub es, Member Env es)
  => GHIssueCreateArgs
  -> Eff es GHIssueCreateResult
ghIssueCreateLogic args =
  withGitHubRepo args.repo
    (\repo -> createIssue CreateIssueInput
        { ciiRepo      = repo
        , ciiTitle     = args.title
        , ciiBody      = fromMaybe "" args.body
        , ciiLabels    = fromMaybe [] args.labels
        , ciiAssignees = fromMaybe [] args.assignees
        })
    (\num -> GHIssueCreateResult
      { number  = num
      , success = True
      , error   = Nothing
      })
    (\err -> GHIssueCreateResult
      { number  = 0
      , success = False
      , error   = Just err
      })


-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE UPDATE TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Core logic for gh_issue_update.
ghIssueUpdateLogic
  :: (Member GitHub es, Member Env es)
  => GHIssueUpdateArgs
  -> Eff es GHIssueUpdateResult
ghIssueUpdateLogic args =
  withGitHubRepo args.repo
    (\repo -> updateIssue repo args.number emptyUpdateIssueInput
        { uiiTitle     = args.title
        , uiiBody      = args.body
        , uiiState     = parseIssueState =<< args.status
        , uiiLabels    = args.labels
        , uiiAssignees = args.assignees
        })
    (\() -> GHIssueUpdateResult
      { success = True
      , number  = args.number
      , error   = Nothing
      })
    (\err -> GHIssueUpdateResult
      { success = False
      , number  = args.number
      , error   = Just err
      })


-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE CLOSE TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Core logic for gh_issue_close.
ghIssueCloseLogic
  :: (Member GitHub es, Member Env es)
  => GHIssueCloseArgs
  -> Eff es GHIssueCloseResult
ghIssueCloseLogic args =
  withGitHubRepo args.repo
    (\repo -> closeIssue repo args.number)
    (\() -> GHIssueCloseResult
      { success = True
      , number  = args.number
      , error   = Nothing
      })
    (\err -> GHIssueCloseResult
      { success = False
      , number  = args.number
      , error   = Just err
      })


-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE REOPEN TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Core logic for gh_issue_reopen.
ghIssueReopenLogic
  :: (Member GitHub es, Member Env es)
  => GHIssueReopenArgs
  -> Eff es GHIssueReopenResult
ghIssueReopenLogic args =
  withGitHubRepo args.repo
    (\repo -> reopenIssue repo args.number)
    (\() -> GHIssueReopenResult
      { success = True
      , number  = args.number
      , error   = Nothing
      })
    (\err -> GHIssueReopenResult
      { success = False
      , number  = args.number
      , error   = Just err
      })


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Parse status text to IssueState.
parseIssueState :: Text -> Maybe IssueState
parseIssueState t = case T.toUpper t of
  "OPEN"   -> Just IssueOpen
  "CLOSED" -> Just IssueClosed
  _        -> Nothing