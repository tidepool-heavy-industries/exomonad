-- | GitHub integration effect
--
-- = ⚠️  STUB IMPLEMENTATION - Not Yet Functional
--
-- This module provides type signatures for GitHub API integration
-- but all runner functions call @error@ at runtime.
--
-- __Intended use:__
--
-- * Type checking and effect composition
-- * Template for implementing real GitHub integration
--
-- __Not suitable for:__
--
-- * Production use
-- * Runtime execution (will crash with @error@)
--
-- == Implementation TODO
--
-- To make this functional, implement a real runner using the GitHub API:
--
-- @
-- runGitHubAPI :: GitHubToken -> Eff (GitHub : es) a -> Eff es a
-- runGitHubAPI token = interpret $ \\case
--   CreateIssue repo title body labels -> do
--     -- Call GitHub REST API
--     ...
--   ListIssues repo labels -> do
--     -- Call GitHub REST API
--     ...
-- @
module Tidepool.Effects.GitHub
  ( -- * Effect
    GitHub(..)
  , createIssue
  , listIssues

    -- * Types
  , IssueUrl(..)
  , Repo(..)
  , Label(..)
  , Issue(..)

    -- * Runner (stub)
  , runGitHubStub
  ) where

import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Control.Monad.Freer (Eff, Member, send, interpret)

import Tidepool.Effect (Log, logInfo)

-- Types

newtype IssueUrl = IssueUrl { unIssueUrl :: Text }
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype Repo = Repo { unRepo :: Text }
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype Label = Label { unLabel :: Text }
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

data Issue = Issue
  { issueUrl    :: IssueUrl
  , issueTitle  :: Text
  , issueLabels :: [Label]
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- Effect

data GitHub r where
  CreateIssue :: Repo -> Text -> Text -> [Label] -> GitHub IssueUrl
  ListIssues  :: Repo -> [Label] -> GitHub [Issue]

createIssue :: Member GitHub effs => Repo -> Text -> Text -> [Label] -> Eff effs IssueUrl
createIssue repo title body labels = send (CreateIssue repo title body labels)

listIssues :: Member GitHub effs => Repo -> [Label] -> Eff effs [Issue]
listIssues repo labels = send (ListIssues repo labels)

-- Stub runner (errors on call)

runGitHubStub :: Member Log effs => Eff (GitHub ': effs) a -> Eff effs a
runGitHubStub = interpret $ \case
  CreateIssue (Repo repo) title _ _ -> do
    logInfo $ "[GitHub:stub] CreateIssue called: " <> repo <> " - " <> title
    error "GitHub.createIssue: not implemented"
  ListIssues (Repo repo) _ -> do
    logInfo $ "[GitHub:stub] ListIssues called: " <> repo
    error "GitHub.listIssues: not implemented"
