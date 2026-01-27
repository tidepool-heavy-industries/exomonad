{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | GitHub MCP tools for centralized issue operations.
--
-- Exposes GitHub operations via MCP so agents can interact with the
-- task tracking system on GitHub.
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
import Data.Aeson
  ( FromJSON(..), ToJSON(..), (.:), (.:?), (.=)
  , object, withObject
  )
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import ExoMonad.Effects.GitHub
  ( GitHub, Repo(..), Issue(..), IssueState(..), IssueFilter(..)
  , CreateIssueInput(..), UpdateIssueInput(..), defaultIssueFilter, emptyUpdateIssueInput
  , listIssues, getIssue, createIssue, updateIssue, closeIssue, reopenIssue
  , GitHubError(..)
  )
import ExoMonad.Effects.Env (Env, getEnv)
import ExoMonad.Graph.Generic (AsHandler, type (:-))
import ExoMonad.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import ExoMonad.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import ExoMonad.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef)
import ExoMonad.Schema
  ( HasJSONSchema(..), objectSchema, arraySchema, emptySchema
  , SchemaType(..), describeField
  )


-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE LIST TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for gh_issue_list tool.
data GHIssueListArgs = GHIssueListArgs
  { gilaRepo   :: Maybe Text   -- ^ owner/repo
  , gilaStatus :: Maybe Text   -- ^ open, closed
  , gilaLabels :: Maybe [Text] -- ^ Filter by labels
  , gilaLimit  :: Maybe Int    -- ^ Max results
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema GHIssueListArgs where
  jsonSchema = objectSchema
    [ ("repo", describeField "repo" "Repository in owner/repo format (optional, uses environment default if omitted)" (emptySchema TString))
    , ("status", describeField "status" "Filter by status: open, closed" (emptySchema TString))
    , ("labels", describeField "labels" "Filter by labels" (arraySchema (emptySchema TString)))
    , ("limit", describeField "limit" "Max results" (emptySchema TNumber))
    ]
    []

instance FromJSON GHIssueListArgs where
  parseJSON = withObject "GHIssueListArgs" $ \v ->
    GHIssueListArgs
      <$> v .:? "repo"
      <*> v .:? "status"
      <*> v .:? "labels"
      <*> v .:? "limit"

instance ToJSON GHIssueListArgs where
  toJSON args = object
    [ "repo"   .= gilaRepo args
    , "status" .= gilaStatus args
    , "labels" .= gilaLabels args
    , "limit"  .= gilaLimit args
    ]

-- | Result of gh_issue_list tool.
data GHIssueListResult = GHIssueListResult
  { gilrIssues :: [Issue]
  , gilrCount  :: Int
  , gilrError  :: Maybe GitHubError
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON GHIssueListResult where
  toJSON res = object $
    [ "issues" .= gilrIssues res
    , "count"  .= gilrCount res
    ] ++ maybe [] (\e -> ["error" .= e]) (gilrError res)

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
  result <- listIssues repo filt
  pure $ gotoExit $ case result of
    Left err -> GHIssueListResult
      { gilrIssues = []
      , gilrCount  = 0
      , gilrError  = Just err
      }
    Right issues -> GHIssueListResult
      { gilrIssues = issues
      , gilrCount  = length issues
      , gilrError  = Nothing
      }


-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE SHOW TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for gh_issue_show tool.
data GHIssueShowArgs = GHIssueShowArgs
  { gisaRepo   :: Maybe Text
  , gisaNumber :: Int
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema GHIssueShowArgs where
  jsonSchema = objectSchema
    [ ("repo", describeField "repo" "Repository in owner/repo format" (emptySchema TString))
    , ("number", describeField "number" "The issue number to show" (emptySchema TNumber))
    ]
    ["number"]

instance FromJSON GHIssueShowArgs where
  parseJSON = withObject "GHIssueShowArgs" $ \v ->
    GHIssueShowArgs
      <$> v .:? "repo"
      <*> v .: "number"

instance ToJSON GHIssueShowArgs where
  toJSON args = object
    [ "repo"   .= gisaRepo args
    , "number" .= gisaNumber args
    ]

-- | Result of gh_issue_show tool.
data GHIssueShowResult = GHIssueShowResult
  { gisrIssue :: Maybe Issue
  , gisrFound :: Bool
  , gisrError :: Maybe GitHubError
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON GHIssueShowResult where
  toJSON res = object $
    [ "issue" .= gisrIssue res
    , "found" .= gisrFound res
    ] ++ maybe [] (\e -> ["error" .= e]) (gisrError res)

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
  result <- getIssue repo args.gisaNumber True -- Include comments
  pure $ gotoExit $ case result of
    Left err -> GHIssueShowResult
      { gisrIssue = Nothing
      , gisrFound = False
      , gisrError = Just err
      }
    Right maybeIssue -> GHIssueShowResult
      { gisrIssue = maybeIssue
      , gisrFound = case maybeIssue of
          Just _ -> True
          Nothing -> False
      , gisrError = Nothing
      }


-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE CREATE TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for gh_issue_create tool.
data GHIssueCreateArgs = GHIssueCreateArgs
  { gcaRepo      :: Maybe Text
  , gcaTitle     :: Text
  , gcaBody      :: Maybe Text
  , gcaLabels    :: Maybe [Text]
  , gcaAssignees :: Maybe [Text]
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema GHIssueCreateArgs where
  jsonSchema = objectSchema
    [ ("repo", describeField "repo" "Repository in owner/repo format" (emptySchema TString))
    , ("title", describeField "title" "Title of the new issue" (emptySchema TString))
    , ("body", describeField "body" "Body description of the issue" (emptySchema TString))
    , ("labels", describeField "labels" "Labels to attach" (arraySchema (emptySchema TString)))
    , ("assignees", describeField "assignees" "Assignee usernames" (arraySchema (emptySchema TString)))
    ]
    ["title"]

instance FromJSON GHIssueCreateArgs where
  parseJSON = withObject "GHIssueCreateArgs" $ \v ->
    GHIssueCreateArgs
      <$> v .:? "repo"
      <*> v .: "title"
      <*> v .:? "body"
      <*> v .:? "labels"
      <*> v .:? "assignees"

instance ToJSON GHIssueCreateArgs where
  toJSON args = object
    [ "repo"      .= gcaRepo args
    , "title"     .= gcaTitle args
    , "body"      .= gcaBody args
    , "labels"    .= gcaLabels args
    , "assignees" .= gcaAssignees args
    ]

-- | Result of gh_issue_create tool.
data GHIssueCreateResult = GHIssueCreateResult
  { gcrNumber  :: Maybe Int
  , gcrSuccess :: Bool
  , gcrError   :: Maybe GitHubError
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON GHIssueCreateResult where
  toJSON res = object $
    [ "number"  .= gcrNumber res
    , "success" .= gcrSuccess res
    ] ++ maybe [] (\e -> ["error" .= e]) (gcrError res)

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
  result <- createIssue input
  pure $ gotoExit $ case result of
    Left err -> GHIssueCreateResult
      { gcrNumber  = Nothing
      , gcrSuccess = False
      , gcrError   = Just err
      }
    Right num -> GHIssueCreateResult
      { gcrNumber  = Just num
      , gcrSuccess = True
      , gcrError   = Nothing
      }


-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE UPDATE TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for gh_issue_update tool.
data GHIssueUpdateArgs = GHIssueUpdateArgs
  { guaRepo      :: Maybe Text
  , guaNumber    :: Int
  , guaTitle     :: Maybe Text
  , guaBody      :: Maybe Text
  , guaStatus    :: Maybe Text     -- ^ open, closed
  , guaLabels    :: Maybe [Text]
  , guaAssignees :: Maybe [Text]
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema GHIssueUpdateArgs where
  jsonSchema = objectSchema
    [ ("repo", describeField "repo" "Repository in owner/repo format" (emptySchema TString))
    , ("number", describeField "number" "The issue number to update" (emptySchema TNumber))
    , ("title", describeField "title" "New title" (emptySchema TString))
    , ("body", describeField "body" "New body description" (emptySchema TString))
    , ("status", describeField "status" "New status: open, closed" (emptySchema TString))
    , ("labels", describeField "labels" "New set of labels (replaces existing)" (arraySchema (emptySchema TString)))
    , ("assignees", describeField "assignees" "New set of assignees (replaces existing)" (arraySchema (emptySchema TString)))
    ]
    ["number"]

instance FromJSON GHIssueUpdateArgs where
  parseJSON = withObject "GHIssueUpdateArgs" $ \v ->
    GHIssueUpdateArgs
      <$> v .:? "repo"
      <*> v .: "number"
      <*> v .:? "title"
      <*> v .:? "body"
      <*> v .:? "status"
      <*> v .:? "labels"
      <*> v .:? "assignees"

instance ToJSON GHIssueUpdateArgs where
  toJSON args = object
    [ "repo"      .= guaRepo args
    , "number"    .= guaNumber args
    , "title"     .= guaTitle args
    , "body"      .= guaBody args
    , "status"    .= guaStatus args
    , "labels"    .= guaLabels args
    , "assignees" .= guaAssignees args
    ]

-- | Result of gh_issue_update tool.
data GHIssueUpdateResult = GHIssueUpdateResult
  { gurSuccess :: Bool
  , gurNumber  :: Int
  , gurError   :: Maybe GitHubError
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON GHIssueUpdateResult where
  toJSON res = object $
    [ "success" .= gurSuccess res
    , "number"  .= gurNumber res
    ] ++ maybe [] (\e -> ["error" .= e]) (gurError res)

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
  result <- updateIssue repo args.guaNumber input
  pure $ gotoExit $ case result of
    Left err -> GHIssueUpdateResult
      { gurSuccess = False
      , gurNumber  = args.guaNumber
      , gurError   = Just err
      }
    Right () -> GHIssueUpdateResult
      { gurSuccess = True
      , gurNumber  = args.guaNumber
      , gurError   = Nothing
      }


-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE CLOSE TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for gh_issue_close tool.
data GHIssueCloseArgs = GHIssueCloseArgs
  { gclaRepo   :: Maybe Text
  , gclaNumber :: Int
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema GHIssueCloseArgs where
  jsonSchema = objectSchema
    [ ("repo", describeField "repo" "Repository in owner/repo format" (emptySchema TString))
    , ("number", describeField "number" "The issue number to close" (emptySchema TNumber))
    ]
    ["number"]

instance FromJSON GHIssueCloseArgs where
  parseJSON = withObject "GHIssueCloseArgs" $ \v ->
    GHIssueCloseArgs
      <$> v .:? "repo"
      <*> v .: "number"

instance ToJSON GHIssueCloseArgs where
  toJSON args = object
    [ "repo"   .= gclaRepo args
    , "number" .= gclaNumber args
    ]

-- | Result of gh_issue_close tool.
data GHIssueCloseResult = GHIssueCloseResult
  { gclrSuccess :: Bool
  , gclrNumber  :: Int
  , gclrError   :: Maybe GitHubError
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON GHIssueCloseResult where
  toJSON res = object $
    [ "success" .= gclrSuccess res
    , "number"  .= gclrNumber res
    ] ++ maybe [] (\e -> ["error" .= e]) (gclrError res)

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
  result <- closeIssue repo args.gclaNumber
  pure $ gotoExit $ case result of
    Left err -> GHIssueCloseResult
      { gclrSuccess = False
      , gclrNumber  = args.gclaNumber
      , gclrError   = Just err
      }
    Right () -> GHIssueCloseResult
      { gclrSuccess = True
      , gclrNumber  = args.gclaNumber
      , gclrError   = Nothing
      }


-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE REOPEN TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for gh_issue_reopen tool.
data GHIssueReopenArgs = GHIssueReopenArgs
  { graRepo   :: Maybe Text
  , graNumber :: Int
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema GHIssueReopenArgs where
  jsonSchema = objectSchema
    [ ("repo", describeField "repo" "Repository in owner/repo format" (emptySchema TString))
    , ("number", describeField "number" "The issue number to reopen" (emptySchema TNumber))
    ]
    ["number"]

instance FromJSON GHIssueReopenArgs where
  parseJSON = withObject "GHIssueReopenArgs" $ \v ->
    GHIssueReopenArgs
      <$> v .:? "repo"
      <*> v .: "number"

instance ToJSON GHIssueReopenArgs where
  toJSON args = object
    [ "repo"   .= graRepo args
    , "number" .= graNumber args
    ]

-- | Result of gh_issue_reopen tool.
data GHIssueReopenResult = GHIssueReopenResult
  { grrSuccess :: Bool
  , grrNumber  :: Int
  , grrError   :: Maybe GitHubError
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON GHIssueReopenResult where
  toJSON res = object $
    [ "success" .= grrSuccess res
    , "number"  .= grrNumber res
    ] ++ maybe [] (\e -> ["error" .= e]) (grrError res)

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
  result <- reopenIssue repo args.graNumber
  pure $ gotoExit $ case result of
    Left err -> GHIssueReopenResult
      { grrSuccess = False
      , grrNumber  = args.graNumber
      , grrError   = Just err
      }
    Right () -> GHIssueReopenResult
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
      pure $ Repo $ fromMaybe "tidepool-heavy-industries/exomonad" mEnvRepo
