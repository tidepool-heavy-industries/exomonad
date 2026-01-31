{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | MCP Tool Argument Parsers
--
-- Parsers for MCP tool call arguments. Extracted from Main.hs for testability.
module ExoMonad.Guest.Parsers
  ( -- * Response types
    MCPCallOutput (..),
    successResult,
    errorResult,

    -- * Parsers
    parseSpawnAgentsArgs,
    parseCleanupAgentsArgs,
    parseGitLogArgs,
    parseGitPathArgs,
    parseReadFileArgs,
    parseWriteFileArgs,
    parseListIssuesArgs,
    parseGetIssueArgs,
    parseListPRsArgs,

    -- * Request types (for parser return types)
    GitHubListIssuesReq (..),
    GitHubGetIssueReq (..),
    GitHubListPRsReq (..),
  )
where

import Data.Aeson (ToJSON (..), Value, object, (.=), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import ExoMonad.Guest.Effects.AgentControl qualified as AC
import GHC.Generics (Generic)

-- ============================================================================
-- MCP Tool Call Output Types
-- ============================================================================

data MCPCallOutput = MCPCallOutput
  { success :: Bool,
    result :: Maybe Value,
    mcpError :: Maybe Text
  }
  deriving (Show, Generic)

instance ToJSON MCPCallOutput where
  toJSON (MCPCallOutput s r e) =
    object
      [ "success" .= s,
        "result" .= r,
        "error" .= e
      ]

successResult :: Value -> MCPCallOutput
successResult v = MCPCallOutput True (Just v) Nothing

errorResult :: Text -> MCPCallOutput
errorResult msg = MCPCallOutput False Nothing (Just msg)

-- ============================================================================
-- Agent Control Parsers
-- ============================================================================

parseSpawnAgentsArgs :: Value -> Parser ([Text], AC.SpawnOptions)
parseSpawnAgentsArgs = Aeson.withObject "spawn_agents args" $ \v -> do
  issues <- v .: "issues"
  owner <- v .: "owner"
  repo <- v .: "repo"
  worktreeDir <- v .:? "worktree_dir"
  pure (issues, AC.SpawnOptions owner repo worktreeDir)

parseCleanupAgentsArgs :: Value -> Parser ([Text], Bool)
parseCleanupAgentsArgs = Aeson.withObject "cleanup_agents args" $ \v -> do
  issues <- v .: "issues"
  force <- v .:? "force" >>= \case
    Nothing -> pure False
    Just f -> pure f
  pure (issues, force)

-- ============================================================================
-- Git Parsers
-- ============================================================================

parseGitLogArgs :: Value -> Parser (Maybe Text, Int)
parseGitLogArgs = Aeson.withObject "git_log args" $ \v -> do
  path <- v .:? "path"
  limit <- v .:? "limit" >>= \case
    Nothing -> pure 10
    Just l -> pure l
  pure (path, limit)

parseGitPathArgs :: Value -> Parser (Maybe Text)
parseGitPathArgs = Aeson.withObject "git args" $ \v -> v .:? "path"

-- ============================================================================
-- File System Parsers
-- ============================================================================

parseReadFileArgs :: Value -> Parser (Text, Int)
parseReadFileArgs = Aeson.withObject "read_file args" $ \v -> do
  path <- v .: "path"
  maxBytes <- v .:? "max_bytes" >>= \case
    Nothing -> pure 0
    Just m -> pure m
  pure (path, maxBytes)

parseWriteFileArgs :: Value -> Parser (Text, Text, Bool)
parseWriteFileArgs = Aeson.withObject "write_file args" $ \v -> do
  path <- v .: "path"
  content <- v .: "content"
  createParents <- v .:? "create_parents" >>= \case
    Nothing -> pure True
    Just c -> pure c
  pure (path, content, createParents)

-- ============================================================================
-- GitHub Parsers
-- ============================================================================

data GitHubListIssuesReq = GitHubListIssuesReq
  { ghliOwner :: Text,
    ghliRepo :: Text,
    ghliState :: Maybe Text,
    ghliLabels :: Maybe [Text]
  }
  deriving (Show, Generic)

instance ToJSON GitHubListIssuesReq where
  toJSON (GitHubListIssuesReq o r s l) =
    object ["owner" .= o, "repo" .= r, "state" .= s, "labels" .= l]

parseListIssuesArgs :: Value -> Parser GitHubListIssuesReq
parseListIssuesArgs = Aeson.withObject "github_list_issues args" $ \v ->
  GitHubListIssuesReq
    <$> v .: "owner"
    <*> v .: "repo"
    <*> v .:? "state"
    <*> v .:? "labels"

data GitHubGetIssueReq = GitHubGetIssueReq
  { ghgiOwner :: Text,
    ghgiRepo :: Text,
    ghgiNumber :: Int
  }
  deriving (Show, Generic)

instance ToJSON GitHubGetIssueReq where
  toJSON (GitHubGetIssueReq o r n) =
    object ["owner" .= o, "repo" .= r, "number" .= n]

parseGetIssueArgs :: Value -> Parser GitHubGetIssueReq
parseGetIssueArgs = Aeson.withObject "github_get_issue args" $ \v ->
  GitHubGetIssueReq
    <$> v .: "owner"
    <*> v .: "repo"
    <*> v .: "number"

data GitHubListPRsReq = GitHubListPRsReq
  { ghlpOwner :: Text,
    ghlpRepo :: Text,
    ghlpState :: Maybe Text,
    ghlpLimit :: Maybe Int
  }
  deriving (Show, Generic)

instance ToJSON GitHubListPRsReq where
  toJSON (GitHubListPRsReq o r s l) =
    object ["owner" .= o, "repo" .= r, "state" .= s, "limit" .= l]

parseListPRsArgs :: Value -> Parser GitHubListPRsReq
parseListPRsArgs = Aeson.withObject "github_list_prs args" $ \v ->
  GitHubListPRsReq
    <$> v .: "owner"
    <*> v .: "repo"
    <*> v .:? "state"
    <*> v .:? "limit"
