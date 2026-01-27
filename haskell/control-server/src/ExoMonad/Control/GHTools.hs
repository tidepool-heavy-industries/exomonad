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
  ( GitHub, Repo(..), Issue(..), IssueState(..), IssueFilter(..)
  , CreateIssueInput(..), UpdateIssueInput(..), defaultIssueFilter, emptyUpdateIssueInput
  , listIssues, getIssue, createIssue, updateIssue, closeIssue, reopenIssue
  )
import ExoMonad.Effects.Env (Env, getEnv)
import ExoMonad.Graph.Generic (AsHandler, type (:-))
import ExoMonad.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import ExoMonad.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import ExoMonad.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef)
import ExoMonad.Schema (deriveMCPTypeWith, defaultMCPOptions, (??), MCPOptions(..), HasJSONSchema(..))

import ExoMonad.Control.GHTools.Types

-- ════════════════════════════════════════════════════════════════════════════
-- DERIVATIONS
-- ════════════════════════════════════════════════════════════════════════════

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "gila" } ''GHIssueListArgs
  [ 'gilaRepo   ?? "Repository in owner/repo format (optional, uses environment default if omitted)"
  , 'gilaStatus ?? "Filter by status: open, closed"
  , 'gilaLabels ?? "Filter by labels"
  , 'gilaLimit  ?? "Max results"
  ])

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "gisa" } ''GHIssueShowArgs
  [ 'gisaRepo   ?? "Repository in owner/repo format"
  , 'gisaNumber ?? "The issue number to show"
  ])

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "gca" } ''GHIssueCreateArgs
  [ 'gcaRepo      ?? "Repository in owner/repo format"
  , 'gcaTitle     ?? "Title of the new issue"
  , 'gcaBody      ?? "Body description of the issue"
  , 'gcaLabels    ?? "Labels to attach"
  , 'gcaAssignees ?? "Assignee usernames"
  ])

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "gua" } ''GHIssueUpdateArgs
  [ 'guaRepo      ?? "Repository in owner/repo format"
  , 'guaNumber    ?? "The issue number to update"
  , 'guaTitle     ?? "New title"
  , 'guaBody      ?? "New body description"
  , 'guaStatus    ?? "New status: open, closed"
  , 'guaLabels    ?? "New set of labels (replaces existing)"
  , 'guaAssignees ?? "New set of assignees (replaces existing)"
  ])

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "gcla" } ''GHIssueCloseArgs
  [ 'gclaRepo   ?? "Repository in owner/repo format"
  , 'gclaNumber ?? "The issue number to close"
  ])

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "gra" } ''GHIssueReopenArgs
  [ 'graRepo   ?? "Repository in owner/repo format"
  , 'graNumber ?? "The issue number to reopen"
  ])


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
  issues <- listIssues repo filt
  pure $ gotoExit $ GHIssueListResult
    { gilrIssues = issues
    , gilrCount  = length issues
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
  maybeIssue <- getIssue repo args.gisaNumber True -- Include comments
  pure $ gotoExit $ GHIssueShowResult
    { gisrIssue = maybeIssue
    , gisrFound = case maybeIssue of
        Just _ -> True
        Nothing -> False
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
  num <- createIssue input
  pure $ gotoExit $ GHIssueCreateResult
    { gcrNumber  = num
    , gcrSuccess = True
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
  updateIssue repo args.guaNumber input
  pure $ gotoExit $ GHIssueUpdateResult
    { gurSuccess = True
    , gurNumber  = args.guaNumber
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
  closeIssue repo args.gclaNumber
  pure $ gotoExit $ GHIssueCloseResult
    { gclrSuccess = True
    , gclrNumber  = args.gclaNumber
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
  reopenIssue repo args.graNumber
  pure $ gotoExit $ GHIssueReopenResult
    { grrSuccess = True
    , grrNumber  = args.graNumber
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