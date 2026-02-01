-- | GitHub tool definitions and handlers.
module ExoMonad.Guest.Tools.GitHub
  ( -- * Tool types
    GitHubGetIssue,
    GitHubListIssues,
    GitHubListPRs,

    -- * Argument types (exported for tests)
    GitHubGetIssueArgs (..),
    GitHubListIssuesArgs (..),
    GitHubListPRsArgs (..),
  )
where

import Data.Aeson (FromJSON, ToJSON, Value, object, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

import ExoMonad.Guest.HostCall
import ExoMonad.Guest.Tool.Class

-- ============================================================================
-- GitHubGetIssue
-- ============================================================================

-- | Get a single GitHub issue with full details.
data GitHubGetIssue

data GitHubGetIssueArgs = GitHubGetIssueArgs
  { ghgiOwner :: Text,
    ghgiRepo :: Text,
    ghgiNumber :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON GitHubGetIssueArgs where
  parseJSON = Aeson.withObject "GitHubGetIssueArgs" $ \v ->
    GitHubGetIssueArgs
      <$> v .: "owner"
      <*> v .: "repo"
      <*> v .: "number"

instance ToJSON GitHubGetIssueArgs where
  toJSON (GitHubGetIssueArgs o r n) =
    object ["owner" .= o, "repo" .= r, "number" .= n]

data GitHubIssueDetailResp = GitHubIssueDetailResp
  { ghidNumber :: Int,
    ghidTitle :: Text,
    ghidBody :: Maybe Text,
    ghidState :: Text,
    ghidLabels :: [Text],
    ghidAssignees :: [Text],
    ghidCreatedAt :: Text,
    ghidUpdatedAt :: Text
  }
  deriving (Show, Generic)

instance FromJSON GitHubIssueDetailResp where
  parseJSON = Aeson.withObject "GitHubIssueDetailResp" $ \v ->
    GitHubIssueDetailResp
      <$> v .: "number"
      <*> v .: "title"
      <*> v .:? "body"
      <*> v .: "state"
      <*> fmap (maybe [] id) (v .:? "labels")
      <*> fmap (maybe [] id) (v .:? "assignees")
      <*> v .: "created_at"
      <*> v .: "updated_at"

instance ToJSON GitHubIssueDetailResp where
  toJSON r =
    object
      [ "number" .= ghidNumber r,
        "title" .= ghidTitle r,
        "body" .= ghidBody r,
        "state" .= ghidState r,
        "labels" .= ghidLabels r,
        "assignees" .= ghidAssignees r,
        "created_at" .= ghidCreatedAt r,
        "updated_at" .= ghidUpdatedAt r
      ]

instance MCPTool GitHubGetIssue where
  type ToolArgs GitHubGetIssue = GitHubGetIssueArgs
  toolName = "github_get_issue"
  toolDescription = "Get a single GitHub issue with full details"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "required" .= (["owner", "repo", "number"] :: [Text]),
        "properties"
          .= object
            [ "owner"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Repository owner (user or org)" :: Text)
                  ],
              "repo"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Repository name" :: Text)
                  ],
              "number"
                .= object
                  [ "type" .= ("integer" :: Text),
                    "description" .= ("Issue number" :: Text)
                  ]
            ]
      ]
  toolHandler args = do
    result <- callHost host_github_get_issue args
    case result of
      Left err -> pure $ errorResult $ T.pack err
      Right (resp :: GitHubIssueDetailResp) -> pure $ successResult $ Aeson.toJSON resp

-- ============================================================================
-- GitHubListIssues
-- ============================================================================

-- | List issues from a GitHub repository.
data GitHubListIssues

data GitHubListIssuesArgs = GitHubListIssuesArgs
  { ghliOwner :: Text,
    ghliRepo :: Text,
    ghliState :: Maybe Text,
    ghliLabels :: Maybe [Text]
  }
  deriving (Show, Eq, Generic)

instance FromJSON GitHubListIssuesArgs where
  parseJSON = Aeson.withObject "GitHubListIssuesArgs" $ \v ->
    GitHubListIssuesArgs
      <$> v .: "owner"
      <*> v .: "repo"
      <*> v .:? "state"
      <*> v .:? "labels"

instance ToJSON GitHubListIssuesArgs where
  toJSON (GitHubListIssuesArgs o r s l) =
    object ["owner" .= o, "repo" .= r, "state" .= s, "labels" .= l]

data GitHubIssue = GitHubIssue
  { ghiNumber :: Int,
    ghiTitle :: Text,
    ghiState :: Text,
    ghiLabels :: [Text],
    ghiCreatedAt :: Text
  }
  deriving (Show, Generic)

instance FromJSON GitHubIssue where
  parseJSON = Aeson.withObject "GitHubIssue" $ \v ->
    GitHubIssue
      <$> v .: "number"
      <*> v .: "title"
      <*> v .: "state"
      <*> fmap (maybe [] id) (v .:? "labels")
      <*> v .: "created_at"

instance ToJSON GitHubIssue where
  toJSON (GitHubIssue n t s l c) =
    object ["number" .= n, "title" .= t, "state" .= s, "labels" .= l, "created_at" .= c]

data GitHubIssuesResp = GitHubIssuesResp {ghIssues :: [GitHubIssue]}
  deriving (Show, Generic)

instance FromJSON GitHubIssuesResp where
  parseJSON = Aeson.withObject "GitHubIssuesResp" $ \v ->
    GitHubIssuesResp <$> v .: "issues"

instance ToJSON GitHubIssuesResp where
  toJSON (GitHubIssuesResp is) = object ["issues" .= is]

instance MCPTool GitHubListIssues where
  type ToolArgs GitHubListIssues = GitHubListIssuesArgs
  toolName = "github_list_issues"
  toolDescription = "List issues from a GitHub repository"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "required" .= (["owner", "repo"] :: [Text]),
        "properties"
          .= object
            [ "owner"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Repository owner (user or org)" :: Text)
                  ],
              "repo"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Repository name" :: Text)
                  ],
              "state"
                .= object
                  [ "type" .= ("string" :: Text),
                    "enum" .= (["open", "closed", "all"] :: [Text]),
                    "description" .= ("Filter by issue state (default: open)" :: Text)
                  ],
              "labels"
                .= object
                  [ "type" .= ("array" :: Text),
                    "items" .= object ["type" .= ("string" :: Text)],
                    "description" .= ("Filter by labels" :: Text)
                  ]
            ]
      ]
  toolHandler args = do
    result <- callHost host_github_list_issues args
    case result of
      Left err -> pure $ errorResult $ T.pack err
      Right (resp :: GitHubIssuesResp) -> pure $ successResult $ Aeson.toJSON resp

-- ============================================================================
-- GitHubListPRs
-- ============================================================================

-- | List pull requests from a GitHub repository.
data GitHubListPRs

data GitHubListPRsArgs = GitHubListPRsArgs
  { ghlpOwner :: Text,
    ghlpRepo :: Text,
    ghlpState :: Maybe Text,
    ghlpLimit :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON GitHubListPRsArgs where
  parseJSON = Aeson.withObject "GitHubListPRsArgs" $ \v ->
    GitHubListPRsArgs
      <$> v .: "owner"
      <*> v .: "repo"
      <*> v .:? "state"
      <*> v .:? "limit"

instance ToJSON GitHubListPRsArgs where
  toJSON (GitHubListPRsArgs o r s l) =
    object ["owner" .= o, "repo" .= r, "state" .= s, "limit" .= l]

data GitHubPR = GitHubPR
  { ghprNumber :: Int,
    ghprTitle :: Text,
    ghprState :: Text,
    ghprHead :: Text,
    ghprBase :: Text,
    ghprCreatedAt :: Text
  }
  deriving (Show, Generic)

instance FromJSON GitHubPR where
  parseJSON = Aeson.withObject "GitHubPR" $ \v ->
    GitHubPR
      <$> v .: "number"
      <*> v .: "title"
      <*> v .: "state"
      <*> v .: "head"
      <*> v .: "base"
      <*> v .: "created_at"

instance ToJSON GitHubPR where
  toJSON (GitHubPR n t s h b c) =
    object ["number" .= n, "title" .= t, "state" .= s, "head" .= h, "base" .= b, "created_at" .= c]

data GitHubPRsResp = GitHubPRsResp {ghPRs :: [GitHubPR]}
  deriving (Show, Generic)

instance FromJSON GitHubPRsResp where
  parseJSON = Aeson.withObject "GitHubPRsResp" $ \v ->
    GitHubPRsResp <$> v .: "prs"

instance ToJSON GitHubPRsResp where
  toJSON (GitHubPRsResp prs) = object ["prs" .= prs]

instance MCPTool GitHubListPRs where
  type ToolArgs GitHubListPRs = GitHubListPRsArgs
  toolName = "github_list_prs"
  toolDescription = "List pull requests from a GitHub repository"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "required" .= (["owner", "repo"] :: [Text]),
        "properties"
          .= object
            [ "owner"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Repository owner (user or org)" :: Text)
                  ],
              "repo"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Repository name" :: Text)
                  ],
              "state"
                .= object
                  [ "type" .= ("string" :: Text),
                    "enum" .= (["open", "closed", "all"] :: [Text]),
                    "description" .= ("Filter by PR state (default: open)" :: Text)
                  ],
              "limit"
                .= object
                  [ "type" .= ("integer" :: Text),
                    "description" .= ("Maximum number of PRs to return" :: Text)
                  ]
            ]
      ]
  toolHandler args = do
    result <- callHost host_github_list_prs args
    case result of
      Left err -> pure $ errorResult $ T.pack err
      Right (resp :: GitHubPRsResp) -> pure $ successResult $ Aeson.toJSON resp
