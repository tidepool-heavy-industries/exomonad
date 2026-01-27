{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- | GitHub MCP tools for centralized issue operations.
module ExoMonad.Control.GHTools
  ( -- * List Tool
    GHIssueListGraph(..)
  , ghIssueListLogic
  , GHIssueListArgs(..)
  , GHIssueListResult(..)

    -- * Show Tool
  , GHIssueShowGraph(..)
  , ghIssueShowLogic
  , GHIssueShowArgs(..)
  , GHIssueShowResult(..)

    -- * Create Tool
  , GHIssueCreateGraph(..)
  , ghIssueCreateLogic
  , GHIssueCreateArgs(..)
  , GHIssueCreateResult(..)

    -- * Update Tool
  , GHIssueUpdateGraph(..)
  , ghIssueUpdateLogic
  , GHIssueUpdateArgs(..)
  , GHIssueUpdateResult(..)

    -- * Close Tool
  , GHIssueCloseGraph(..)
  , ghIssueCloseLogic
  , GHIssueCloseArgs(..)
  , GHIssueCloseResult(..)

    -- * Reopen Tool
  , GHIssueReopenGraph(..)
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
import GHC.Generics (Generic)

import ExoMonad.Effects.GitHub
  ( GitHub, Repo(..), IssueState(..), IssueFilter(..)
  , CreateIssueInput(..), UpdateIssueInput(..), defaultIssueFilter, emptyUpdateIssueInput
  , listIssues, getIssue, createIssue, updateIssue, closeIssue, reopenIssue
  )
import ExoMonad.Effects.Env (Env, getEnv)
import ExoMonad.Graph.Generic (type (:-))
import ExoMonad.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import ExoMonad.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import ExoMonad.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef)

import ExoMonad.Control.GHTools.Types

-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE LIST TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Graph definition for gh_issue_list tool.
data GHIssueListGraph mode = GHIssueListGraph
  { gilEntry :: mode :- EntryNode GHIssueListArgs
      :@ MCPExport
      :@ MCPToolDef '("gh_issue_list", "List GitHub issues with optional status/label filters.")

  , gilRun :: mode :- LogicNode
      :@ Input GHIssueListArgs
      :@ UsesEffects '[GitHub, Env, Goto Exit GHIssueListResult]

  , gilExit :: mode :- ExitNode GHIssueListResult
  }
  deriving Generic

-- | Core logic for gh_issue_list.
ghIssueListLogic
  :: (Member GitHub es, Member Env es)
  => GHIssueListArgs
  -> Eff es (GotoChoice '[To Exit GHIssueListResult])
ghIssueListLogic args = do
  repo <- getRepo args.gilaRepo
  let filt = defaultIssueFilter
        { ifLabels = fromMaybe [] args.gilaLabels
        , ifState  = parseIssueState =<< args.gilaStatus
        , ifLimit  = args.gilaLimit
        }
  res <- listIssues repo filt
  case res of
    Left err -> pure $ gotoExit $ GHIssueListResult
      { gilrIssues = []
      , gilrCount  = 0
      , gilrError  = Just (T.pack $ show err)
      }
    Right issues -> pure $ gotoExit $ GHIssueListResult
      { gilrIssues = issues
      , gilrCount  = length issues
      , gilrError  = Nothing
      }


-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE SHOW TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Graph definition for gh_issue_show tool.
data GHIssueShowGraph mode = GHIssueShowGraph
  { gisEntry :: mode :- EntryNode GHIssueShowArgs
      :@ MCPExport
      :@ MCPToolDef '("gh_issue_show", "Get detailed information about a specific GitHub issue by number.")

  , gisRun :: mode :- LogicNode
      :@ Input GHIssueShowArgs
      :@ UsesEffects '[GitHub, Env, Goto Exit GHIssueShowResult]

  , gisExit :: mode :- ExitNode GHIssueShowResult
  }
  deriving Generic

-- | Core logic for gh_issue_show.
ghIssueShowLogic
  :: (Member GitHub es, Member Env es)
  => GHIssueShowArgs
  -> Eff es (GotoChoice '[To Exit GHIssueShowResult])
ghIssueShowLogic args = do
  repo <- getRepo args.gisaRepo
  res <- getIssue repo args.gisaNumber True -- Include comments
  case res of
    Left err -> pure $ gotoExit $ GHIssueShowResult
      { gisrIssue = Nothing
      , gisrFound = False
      , gisrError = Just (T.pack $ show err)
      }
    Right maybeIssue -> pure $ gotoExit $ GHIssueShowResult
      { gisrIssue = maybeIssue
      , gisrFound = case maybeIssue of
          Just _ -> True
          Nothing -> False
      , gisrError = Nothing
      }


-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE CREATE TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Graph definition for gh_issue_create tool.
data GHIssueCreateGraph mode = GHIssueCreateGraph
  { gcEntry :: mode :- EntryNode GHIssueCreateArgs
      :@ MCPExport
      :@ MCPToolDef '("gh_issue_create", "Create a new GitHub issue.")

  , gcRun :: mode :- LogicNode
      :@ Input GHIssueCreateArgs
      :@ UsesEffects '[GitHub, Env, Goto Exit GHIssueCreateResult]

  , gcExit :: mode :- ExitNode GHIssueCreateResult
  }
  deriving Generic

-- | Core logic for gh_issue_create.
ghIssueCreateLogic
  :: (Member GitHub es, Member Env es)
  => GHIssueCreateArgs
  -> Eff es (GotoChoice '[To Exit GHIssueCreateResult])
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
    Left err -> pure $ gotoExit $ GHIssueCreateResult
      { gcrNumber  = 0
      , gcrSuccess = False
      , gcrError   = Just (T.pack $ show err)
      }
    Right num -> pure $ gotoExit $ GHIssueCreateResult
      { gcrNumber  = num
      , gcrSuccess = True
      , gcrError   = Nothing
      }


-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE UPDATE TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Graph definition for gh_issue_update tool.
data GHIssueUpdateGraph mode = GHIssueUpdateGraph
  { guEntry :: mode :- EntryNode GHIssueUpdateArgs
      :@ MCPExport
      :@ MCPToolDef '("gh_issue_update", "Update fields of a GitHub issue.")

  , guRun :: mode :- LogicNode
      :@ Input GHIssueUpdateArgs
      :@ UsesEffects '[GitHub, Env, Goto Exit GHIssueUpdateResult]

  , guExit :: mode :- ExitNode GHIssueUpdateResult
  }
  deriving Generic

-- | Core logic for gh_issue_update.
ghIssueUpdateLogic
  :: (Member GitHub es, Member Env es)
  => GHIssueUpdateArgs
  -> Eff es (GotoChoice '[To Exit GHIssueUpdateResult])
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
    Left err -> pure $ gotoExit $ GHIssueUpdateResult
      { gurSuccess = False
      , gurNumber  = args.guaNumber
      , gurError   = Just (T.pack $ show err)
      }
    Right () -> pure $ gotoExit $ GHIssueUpdateResult
      { gurSuccess = True
      , gurNumber  = args.guaNumber
      , gurError   = Nothing
      }


-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE CLOSE TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Graph definition for gh_issue_close tool.
data GHIssueCloseGraph mode = GHIssueCloseGraph
  { gclEntry :: mode :- EntryNode GHIssueCloseArgs
      :@ MCPExport
      :@ MCPToolDef '("gh_issue_close", "Close a GitHub issue.")

  , gclRun :: mode :- LogicNode
      :@ Input GHIssueCloseArgs
      :@ UsesEffects '[GitHub, Env, Goto Exit GHIssueCloseResult]

  , gclExit :: mode :- ExitNode GHIssueCloseResult
  }
  deriving Generic

-- | Core logic for gh_issue_close.
ghIssueCloseLogic
  :: (Member GitHub es, Member Env es)
  => GHIssueCloseArgs
  -> Eff es (GotoChoice '[To Exit GHIssueCloseResult])
ghIssueCloseLogic args = do
  repo <- getRepo args.gclaRepo
  res <- closeIssue repo args.gclaNumber
  case res of
    Left err -> pure $ gotoExit $ GHIssueCloseResult
      { gclrSuccess = False
      , gclrNumber  = args.gclaNumber
      , gclrError   = Just (T.pack $ show err)
      }
    Right () -> pure $ gotoExit $ GHIssueCloseResult
      { gclrSuccess = True
      , gclrNumber  = args.gclaNumber
      , gclrError   = Nothing
      }


-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE REOPEN TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Graph definition for gh_issue_reopen tool.
data GHIssueReopenGraph mode = GHIssueReopenGraph
  { greEntry :: mode :- EntryNode GHIssueReopenArgs
      :@ MCPExport
      :@ MCPToolDef '("gh_issue_reopen", "Reopen a closed GitHub issue.")

  , greRun :: mode :- LogicNode
      :@ Input GHIssueReopenArgs
      :@ UsesEffects '[GitHub, Env, Goto Exit GHIssueReopenResult]

  , greExit :: mode :- ExitNode GHIssueReopenResult
  }
  deriving Generic

-- | Core logic for gh_issue_reopen.
ghIssueReopenLogic
  :: (Member GitHub es, Member Env es)
  => GHIssueReopenArgs
  -> Eff es (GotoChoice '[To Exit GHIssueReopenResult])
ghIssueReopenLogic args = do
  repo <- getRepo args.graRepo
  res <- reopenIssue repo args.graNumber
  case res of
    Left err -> pure $ gotoExit $ GHIssueReopenResult
      { grrSuccess = False
      , grrNumber  = args.graNumber
      , grrError   = Just (T.pack $ show err)
      }
    Right () -> pure $ gotoExit $ GHIssueReopenResult
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