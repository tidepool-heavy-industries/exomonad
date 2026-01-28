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
  ( GitHub, Repo(..), IssueState(..), IssueFilter(..)
  , CreateIssueInput(..), UpdateIssueInput(..), defaultIssueFilter, emptyUpdateIssueInput
  , listIssues, getIssue, createIssue, updateIssue, closeIssue, reopenIssue
  )
import ExoMonad.Effects.Env (Env, getEnv)

import ExoMonad.Control.GHTools.Types

-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE LIST TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Core logic for gh_issue_list.
ghIssueListLogic
  :: (Member GitHub es, Member Env es)
  => GHIssueListArgs
  -> Eff es GHIssueListResult
ghIssueListLogic args = do
  repo <- getRepo args.repo
  let filt = defaultIssueFilter
        { ifLabels = fromMaybe [] args.labels
        , ifState  = parseIssueState =<< args.status
        , ifLimit  = args.limit
        }
  res <- listIssues repo filt
  case res of
    Left err -> pure $ GHIssueListResult
      { issues = []
      , count  = 0
      , error  = Just (T.pack $ show err)
      }
    Right is -> pure $ GHIssueListResult
      { issues = is
      , count  = length is
      , error  = Nothing
      }


-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE SHOW TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Core logic for gh_issue_show.
ghIssueShowLogic
  :: (Member GitHub es, Member Env es)
  => GHIssueShowArgs
  -> Eff es GHIssueShowResult
ghIssueShowLogic args = do
  repo <- getRepo args.repo
  res <- getIssue repo args.number True -- Include comments
  case res of
    Left err -> pure $ GHIssueShowResult
      { issue = Nothing
      , found = False
      , error = Just (T.pack $ show err)
      }
    Right maybeIssue -> pure $ GHIssueShowResult
      { issue = maybeIssue
      , found = case maybeIssue of
          Just _ -> True
          Nothing -> False
      , error = Nothing
      }


-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE CREATE TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Core logic for gh_issue_create.
ghIssueCreateLogic
  :: (Member GitHub es, Member Env es)
  => GHIssueCreateArgs
  -> Eff es GHIssueCreateResult
ghIssueCreateLogic args = do
  repo <- getRepo args.repo
  let input = CreateIssueInput
        { ciiRepo      = repo
        , ciiTitle     = args.title
        , ciiBody      = fromMaybe "" args.body
        , ciiLabels    = fromMaybe [] args.labels
        , ciiAssignees = fromMaybe [] args.assignees
        }
  res <- createIssue input
  case res of
    Left err -> pure $ GHIssueCreateResult
      { number  = 0
      , success = False
      , error   = Just (T.pack $ show err)
      }
    Right num -> pure $ GHIssueCreateResult
      { number  = num
      , success = True
      , error   = Nothing
      }


-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE UPDATE TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Core logic for gh_issue_update.
ghIssueUpdateLogic
  :: (Member GitHub es, Member Env es)
  => GHIssueUpdateArgs
  -> Eff es GHIssueUpdateResult
ghIssueUpdateLogic args = do
  repo <- getRepo args.repo
  let input = emptyUpdateIssueInput
        { uiiTitle     = args.title
        , uiiBody      = args.body
        , uiiState     = parseIssueState =<< args.status
        , uiiLabels    = args.labels
        , uiiAssignees = args.assignees
        }
  res <- updateIssue repo args.number input
  case res of
    Left err -> pure $ GHIssueUpdateResult
      { success = False
      , number  = args.number
      , error   = Just (T.pack $ show err)
      }
    Right () -> pure $ GHIssueUpdateResult
      { success = True
      , number  = args.number
      , error   = Nothing
      }


-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE CLOSE TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Core logic for gh_issue_close.
ghIssueCloseLogic
  :: (Member GitHub es, Member Env es)
  => GHIssueCloseArgs
  -> Eff es GHIssueCloseResult
ghIssueCloseLogic args = do
  repo <- getRepo args.repo
  res <- closeIssue repo args.number
  case res of
    Left err -> pure $ GHIssueCloseResult
      { success = False
      , number  = args.number
      , error   = Just (T.pack $ show err)
      }
    Right () -> pure $ GHIssueCloseResult
      { success = True
      , number  = args.number
      , error   = Nothing
      }


-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE REOPEN TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Core logic for gh_issue_reopen.
ghIssueReopenLogic
  :: (Member GitHub es, Member Env es)
  => GHIssueReopenArgs
  -> Eff es GHIssueReopenResult
ghIssueReopenLogic args = do
  repo <- getRepo args.repo
  res <- reopenIssue repo args.number
  case res of
    Left err -> pure $ GHIssueReopenResult
      { success = False
      , number  = args.number
      , error   = Just (T.pack $ show err)
      }
    Right () -> pure $ GHIssueReopenResult
      { success = True
      , number  = args.number
      , error   = Nothing
      }


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Parse status text to IssueState.
parseIssueState :: Text -> Maybe IssueState
parseIssueState t = case T.toUpper t of
  "OPEN"   -> Just IssueOpen
  "CLOSED" -> Just IssueClosed
  _        -> Nothing

-- | Get repository from args or environment.
getRepo :: Member Env es => Maybe Text -> Eff es Repo
getRepo mRepo = do
  case mRepo of
    Just r -> pure $ Repo r
    Nothing -> do
      mEnvRepo <- getEnv "GITHUB_REPO"
      pure $ Repo $ fromMaybe "exomonad-ai/exomonad" mEnvRepo