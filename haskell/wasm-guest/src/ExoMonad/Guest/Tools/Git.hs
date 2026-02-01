-- | Git tool definitions and handlers.
module ExoMonad.Guest.Tools.Git
  ( -- * Tool types
    GitBranch,
    GitStatus,
    GitLog,

    -- * Argument types (exported for tests)
    GitBranchArgs (..),
    GitStatusArgs (..),
    GitLogArgs (..),
  )
where

import Data.Aeson (FromJSON, ToJSON, Value, object, (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

import ExoMonad.Guest.HostCall
import ExoMonad.Guest.Tool.Class

-- ============================================================================
-- GitBranch
-- ============================================================================

-- | Get the current git branch.
data GitBranch

data GitBranchArgs = GitBranchArgs
  { gbPath :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON GitBranchArgs where
  parseJSON = Aeson.withObject "GitBranchArgs" $ \v ->
    GitBranchArgs <$> v .:? "path"

data GitBranchReq = GitBranchReq
  deriving (Show, Generic)

instance ToJSON GitBranchReq

data GitBranchResp = GitBranchResp {branch :: Text}
  deriving (Show, Generic)

instance FromJSON GitBranchResp

instance MCPTool GitBranch where
  type ToolArgs GitBranch = GitBranchArgs
  toolName = "git_branch"
  toolDescription = "Get the current git branch name"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "path"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Directory path (defaults to project root)" :: Text)
                  ]
            ]
      ]
  toolHandler _args = do
    res <- callHost host_git_get_branch GitBranchReq
    case res of
      Left err -> pure $ errorResult $ T.pack err
      Right (GitBranchResp b) -> pure $ successResult $ Aeson.toJSON b

-- ============================================================================
-- GitStatus
-- ============================================================================

-- | Get list of modified/untracked files.
data GitStatus

data GitStatusArgs = GitStatusArgs
  { gsPath :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON GitStatusArgs where
  parseJSON = Aeson.withObject "GitStatusArgs" $ \v ->
    GitStatusArgs <$> v .:? "path"

data GitDirtyFilesReq = GitDirtyFilesReq {gdfPath :: Maybe Text}
  deriving (Show, Generic)

instance ToJSON GitDirtyFilesReq where
  toJSON (GitDirtyFilesReq p) = object ["path" .= p]

data GitDirtyFilesResp = GitDirtyFilesResp {files :: [Text], count :: Int}
  deriving (Show, Generic)

instance FromJSON GitDirtyFilesResp where
  parseJSON = Aeson.withObject "GitDirtyFilesResp" $ \v ->
    GitDirtyFilesResp
      <$> fmap (maybe [] id) (v .:? "files")
      <*> fmap (maybe 0 id) (v .:? "count")

instance ToJSON GitDirtyFilesResp

instance MCPTool GitStatus where
  type ToolArgs GitStatus = GitStatusArgs
  toolName = "git_status"
  toolDescription = "Get list of modified/untracked files (git status --porcelain)"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "path"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Directory path (defaults to project root)" :: Text)
                  ]
            ]
      ]
  toolHandler args = do
    res <- callHost host_git_get_dirty_files (GitDirtyFilesReq (gsPath args))
    case res of
      Left err -> pure $ errorResult $ T.pack err
      Right (resp :: GitDirtyFilesResp) -> pure $ successResult $ Aeson.toJSON resp

-- ============================================================================
-- GitLog
-- ============================================================================

-- | Get recent git commits.
data GitLog

data GitLogArgs = GitLogArgs
  { glPath :: Maybe Text,
    glLimit :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON GitLogArgs where
  parseJSON = Aeson.withObject "GitLogArgs" $ \v ->
    GitLogArgs
      <$> v .:? "path"
      <*> v .:? "limit"

data GitLogReq = GitLogReq {glrPath :: Maybe Text, glrLimit :: Int}
  deriving (Show, Generic)

instance ToJSON GitLogReq where
  toJSON (GitLogReq p l) = object ["path" .= p, "limit" .= l]

data GitLogResp = GitLogResp {commits :: [GitCommit]}
  deriving (Show, Generic)

instance FromJSON GitLogResp
instance ToJSON GitLogResp

data GitCommit = GitCommit
  { gcHash :: Text,
    gcMessage :: Text,
    gcAuthor :: Text,
    gcDate :: Text
  }
  deriving (Show, Generic)

instance FromJSON GitCommit where
  parseJSON = Aeson.withObject "GitCommit" $ \v ->
    GitCommit
      <$> fmap (maybe "" id) (v .:? "hash")
      <*> fmap (maybe "" id) (v .:? "message")
      <*> fmap (maybe "" id) (v .:? "author")
      <*> fmap (maybe "" id) (v .:? "date")

instance ToJSON GitCommit where
  toJSON (GitCommit h m a d) =
    object ["hash" .= h, "message" .= m, "author" .= a, "date" .= d]

instance MCPTool GitLog where
  type ToolArgs GitLog = GitLogArgs
  toolName = "git_log"
  toolDescription = "Get recent git commits"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "path"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Directory path (defaults to project root)" :: Text)
                  ],
              "limit"
                .= object
                  [ "type" .= ("integer" :: Text),
                    "description" .= ("Number of commits to show (default: 10)" :: Text),
                    "default" .= (10 :: Int)
                  ]
            ]
      ]
  toolHandler args = do
    let limit = maybe 10 id (glLimit args)
    res <- callHost host_git_get_recent_commits (GitLogReq (glPath args) limit)
    case res of
      Left err -> pure $ errorResult $ T.pack err
      Right (resp :: GitLogResp) -> pure $ successResult $ Aeson.toJSON resp
