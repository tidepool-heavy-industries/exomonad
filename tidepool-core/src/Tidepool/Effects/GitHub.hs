-- | GitHub integration effect
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
import Effectful
import Effectful.Dispatch.Dynamic

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

data GitHub :: Effect where
  CreateIssue :: Repo -> Text -> Text -> [Label] -> GitHub m IssueUrl
  ListIssues  :: Repo -> [Label] -> GitHub m [Issue]

type instance DispatchOf GitHub = 'Dynamic

createIssue :: GitHub :> es => Repo -> Text -> Text -> [Label] -> Eff es IssueUrl
createIssue repo title body labels = send (CreateIssue repo title body labels)

listIssues :: GitHub :> es => Repo -> [Label] -> Eff es [Issue]
listIssues repo labels = send (ListIssues repo labels)

-- Stub runner (errors on call)

runGitHubStub :: (IOE :> es, Log :> es) => Eff (GitHub : es) a -> Eff es a
runGitHubStub = interpret $ \_ -> \case
  CreateIssue (Repo repo) title _ _ -> do
    logInfo $ "[GitHub:stub] CreateIssue called: " <> repo <> " - " <> title
    error "GitHub.createIssue: not implemented"
  ListIssues (Repo repo) _ -> do
    logInfo $ "[GitHub:stub] ListIssues called: " <> repo
    error "GitHub.listIssues: not implemented"
