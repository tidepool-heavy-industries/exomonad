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
  repo <- getRepo args.gilaRepo
  let filt = defaultIssueFilter
        { ifLabels = fromMaybe [] args.gilaLabels
        , ifState  = parseIssueState =<< args.gilaStatus
        , ifLimit  = args.gilaLimit
        }
  res <- listIssues repo filt
  case res of
    Left err -> pure $ GHIssueListResult
      { gilrIssues = []
      , gilrCount  = 0
      , gilrError  = Just (T.pack $ show err)
      }
    Right issues -> pure $ GHIssueListResult
      { gilrIssues = issues
      , gilrCount  = length issues
      , gilrError  = Nothing
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
  repo <- getRepo args.gisaRepo
  res <- getIssue repo args.gisaNumber True -- Include comments
  case res of
    Left err -> pure $ GHIssueShowResult
      { gisrIssue = Nothing
      , gisrFound = False
      , gisrError = Just (T.pack $ show err)
      }
    Right maybeIssue -> pure $ GHIssueShowResult
      { gisrIssue = maybeIssue
      , gisrFound = case maybeIssue of
          Just _ -> True
          Nothing -> False
      , gisrError = Nothing
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
  repo <- getRepo args.gcaRepo
  let input = CreateIssueInput
        { ciiRepo      = repo
        , ciiTitle     = args.gcaTitle
        , ciiBody      = fromMaybe "" args.gcaBody
        , ciiLabels    = fromMaybe [] args.gcaLabels
        , ciiAssignees = fromMaybe [] args.gcaAssignees
        }
  res <- createIssue input
  case res of
    Left err -> pure $ GHIssueCreateResult
      { gcrNumber  = 0
      , gcrSuccess = False
      , gcrError   = Just (T.pack $ show err)
      }
    Right num -> pure $ GHIssueCreateResult
      { gcrNumber  = num
      , gcrSuccess = True
      , gcrError   = Nothing
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
  repo <- getRepo args.guaRepo
  let input = emptyUpdateIssueInput
        { uiiTitle     = args.guaTitle
        , uiiBody      = args.guaBody
        , uiiState     = parseIssueState =<< args.guaStatus
        , uiiLabels    = args.guaLabels
        , uiiAssignees = args.guaAssignees
        }
  res <- updateIssue repo args.guaNumber input
  case res of
    Left err -> pure $ GHIssueUpdateResult
      { gurSuccess = False
      , gurNumber  = args.guaNumber
      , gurError   = Just (T.pack $ show err)
      }
    Right () -> pure $ GHIssueUpdateResult
      { gurSuccess = True
      , gurNumber  = args.guaNumber
      , gurError   = Nothing
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
  repo <- getRepo args.gclaRepo
  res <- closeIssue repo args.gclaNumber
  case res of
    Left err -> pure $ GHIssueCloseResult
      { gclrSuccess = False
      , gclrNumber  = args.gclaNumber
      , gclrError   = Just (T.pack $ show err)
      }
    Right () -> pure $ GHIssueCloseResult
      { gclrSuccess = True
      , gclrNumber  = args.gclaNumber
      , gclrError   = Nothing
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
  repo <- getRepo args.graRepo
  res <- reopenIssue repo args.graNumber
  case res of
    Left err -> pure $ GHIssueReopenResult
      { grrSuccess = False
      , grrNumber  = args.graNumber
      , grrError   = Just (T.pack $ show err)
      }
    Right () -> pure $ GHIssueReopenResult
      { grrSuccess = True
      , grrNumber  = args.graNumber
      , grrError   = Nothing
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