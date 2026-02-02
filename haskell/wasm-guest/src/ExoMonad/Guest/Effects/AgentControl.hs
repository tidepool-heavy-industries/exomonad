{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | High-level agent control effects.
--
-- These effects provide semantic operations for agent lifecycle management.
-- The Rust host handles all I/O (git, zellij, filesystem).
module ExoMonad.Guest.Effects.AgentControl
  ( -- * Effect type
    AgentControl (..),

    -- * Smart constructors
    spawnAgent,
    spawnAgents,
    cleanupAgent,
    cleanupAgents,
    listAgents,

    -- * Interpreter
    runAgentControl,

    -- * Types
    AgentType (..),
    SpawnOptions (..),
    SpawnResult (..),
    AgentInfo (..),
    BatchSpawnResult (..),
    BatchCleanupResult (..),
    HostResult (..),
    SpawnAgentInput (..),
    SpawnAgentsInput (..),
    CleanupAgentInput (..),
    CleanupAgentsInput (..),
  )
where

import Polysemy (Sem, Member, interpret, embed, send)
import Polysemy.Embed (Embed)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, withText, (.:), (.:?), (.=))
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Text qualified
import qualified Data.Text.Read as TR
import Data.Word (Word64)
import ExoMonad.Guest.HostCall (callHost, host_agent_cleanup, host_agent_cleanup_batch, host_agent_list, host_agent_spawn, host_agent_spawn_batch)
import GHC.Generics (Generic)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)

-- ============================================================================
-- Types (match Rust agent_control.rs)
-- ============================================================================

-- | Agent type for spawned agents.
data AgentType = Claude | Gemini
  deriving (Show, Eq, Generic)

instance ToJSON AgentType where
  toJSON Claude = "claude"
  toJSON Gemini = "gemini"

instance FromJSON AgentType where
  parseJSON = withText "AgentType" $ \case
    "claude" -> pure Claude
    "gemini" -> pure Gemini
    other -> fail $ "Invalid agent type: " <> Data.Text.unpack other

-- | Options for spawning an agent.
data SpawnOptions = SpawnOptions
  { owner :: Text,
    repo :: Text,
    worktreeDir :: Maybe Text,
    agentType :: Maybe AgentType
  }
  deriving (Show, Eq, Generic)

instance ToJSON SpawnOptions where
  toJSON (SpawnOptions o r w a) =
    object
      [ "owner" .= o,
        "repo" .= r,
        "worktree_dir" .= w,
        "agent_type" .= a
      ]

-- | Result of spawning an agent.
data SpawnResult = SpawnResult
  { worktreePath :: Text,
    branchName :: Text,
    tabName :: Text,
    issueTitle :: Text,
    agentTypeResult :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON SpawnResult where
  parseJSON = withObject "SpawnResult" $ \v ->
    SpawnResult
      <$> v .: "worktree_path"
      <*> v .: "branch_name"
      <*> v .: "tab_name"
      <*> v .: "issue_title"
      <*> v .: "agent_type"

instance ToJSON SpawnResult where
  toJSON (SpawnResult w b t i a) =
    object
      [ "worktree_path" .= w,
        "branch_name" .= b,
        "tab_name" .= t,
        "issue_title" .= i,
        "agent_type" .= a
      ]

-- | Information about an active agent.
data AgentInfo = AgentInfo
  { agentIssueId :: Text,
    agentWorktreePath :: Text,
    agentBranchName :: Text,
    agentHasChanges :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON AgentInfo where
  parseJSON = withObject "AgentInfo" $ \v ->
    AgentInfo
      <$> v .: "issue_id"
      <*> v .: "worktree_path"
      <*> v .: "branch_name"
      <*> v .: "has_changes"

instance ToJSON AgentInfo where
  toJSON (AgentInfo i w b h) =
    object
      [ "issue_id" .= i,
        "worktree_path" .= w,
        "branch_name" .= b,
        "has_changes" .= h
      ]

-- | Result of batch spawn operation.
data BatchSpawnResult = BatchSpawnResult
  { spawned :: [SpawnResult],
    spawnFailed :: [(Text, Text)] -- (issue_id, error)
  }
  deriving (Show, Eq, Generic)

instance FromJSON BatchSpawnResult where
  parseJSON = withObject "BatchSpawnResult" $ \v ->
    BatchSpawnResult
      <$> v .: "spawned"
      <*> v .: "failed"

instance ToJSON BatchSpawnResult where
  toJSON (BatchSpawnResult s f) =
    object
      [ "spawned" .= s,
        "failed" .= f
      ]

-- | Result of batch cleanup operation.
data BatchCleanupResult = BatchCleanupResult
  { cleaned :: [Text],
    cleanupFailed :: [(Text, Text)] -- (issue_id, error)
  }
  deriving (Show, Eq, Generic)

instance FromJSON BatchCleanupResult where
  parseJSON = withObject "BatchCleanupResult" $ \v ->
    BatchCleanupResult
      <$> v .: "cleaned"
      <*> v .: "failed"

instance ToJSON BatchCleanupResult where
  toJSON (BatchCleanupResult c f) =
    object
      [ "cleaned" .= c,
        "failed" .= f
      ]

-- | Host result wrapper (matches Rust HostResult).
data HostResult a
  = Success a
  | HostError Text
  deriving (Show, Eq, Generic)

instance (ToJSON a) => ToJSON (HostResult a) where
  toJSON (Success payload) =
    object
      [ "kind" .= ("Success" :: Text),
        "payload" .= payload
      ]
  toJSON (HostError msg) =
    object
      [ "kind" .= ("Error" :: Text),
        "payload" .= object ["message" .= msg]
      ]

instance (FromJSON a) => FromJSON (HostResult a) where
  parseJSON = withObject "HostResult" $ \v -> do
    kind <- v .: "kind" :: (FromJSON (Maybe Text)) => Data.Aeson.Types.Parser Text
    case kind of
      "Success" -> Success <$> v .: "payload"
      "Error" -> do
        errObj <- v .: "payload"
        HostError <$> (errObj .: "message")
      _ -> fail "Unknown HostResult kind"

-- ============================================================================
-- Input types (for serialization to host)
-- ============================================================================

data SpawnAgentInput = SpawnAgentInput
  { saiIssueId :: Word64,
    saiOwner :: Text,
    saiRepo :: Text,
    saiWorktreeDir :: Maybe Text,
    saiAgentType :: AgentType
  }
  deriving (Show, Eq, Generic)

instance ToJSON SpawnAgentInput where
  toJSON (SpawnAgentInput i o r w a) =
    object
      [ "issue_id" .= i,
        "owner" .= o,
        "repo" .= r,
        "worktree_dir" .= w,
        "agent_type" .= a
      ]
instance FromJSON SpawnAgentInput where
  parseJSON = withObject "SpawnAgentInput" $ \v ->
    SpawnAgentInput
      <$> v .: "issue_id"
      <*> v .: "owner"
      <*> v .: "repo"
      <*> v .: "worktree_dir"
      <*> v .: "agent_type"

data SpawnAgentsInput = SpawnAgentsInput
  { sasIssueIds :: [Text],
    sasOwner :: Text,
    sasRepo :: Text,
    sasWorktreeDir :: Maybe Text,
    sasAgentType :: AgentType
  }
  deriving (Show, Eq, Generic)

instance ToJSON SpawnAgentsInput where
  toJSON (SpawnAgentsInput is o r w a) =
    object
      [ "issue_ids" .= is,
        "owner" .= o,
        "repo" .= r,
        "worktree_dir" .= w,
        "agent_type" .= a
      ]
instance FromJSON SpawnAgentsInput where
  parseJSON = withObject "SpawnAgentsInput" $ \v ->
    SpawnAgentsInput
      <$> v .: "issue_ids"
      <*> v .: "owner"
      <*> v .: "repo"
      <*> v .: "worktree_dir"
      <*> v .: "agent_type"

data CleanupAgentInput = CleanupAgentInput
  { caiIssueId :: Text,
    caiForce :: Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON CleanupAgentInput where
  toJSON (CleanupAgentInput i f) =
    object
      [ "issue_id" .= i,
        "force" .= f
      ]
instance FromJSON CleanupAgentInput where
  parseJSON = withObject "CleanupAgentInput" $ \v ->
    CleanupAgentInput
      <$> v .: "issue_id"
      <*> v .: "force"

data CleanupAgentsInput = CleanupAgentsInput
  { casIssueIds :: [Text],
    casForce :: Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON CleanupAgentsInput where
  toJSON (CleanupAgentsInput is f) =
    object
      [ "issue_ids" .= is,
        "force" .= f
      ]
instance FromJSON CleanupAgentsInput where
  parseJSON = withObject "CleanupAgentsInput" $ \v ->
    CleanupAgentsInput
      <$> v .: "issue_ids"
      <*> v .: "force"

data ListAgentsInput = ListAgentsInput
  deriving (Show, Generic)

instance ToJSON ListAgentsInput where
  toJSON ListAgentsInput = object []

-- ============================================================================
-- Effect type
-- ============================================================================

-- | Agent control effect for high-level agent lifecycle management.
data AgentControl m a where
  -- | Spawn an agent for a GitHub issue.
  SpawnAgent :: Text -> SpawnOptions -> AgentControl m (Either Text SpawnResult)
  -- | Spawn multiple agents.
  SpawnAgents :: [Text] -> SpawnOptions -> AgentControl m BatchSpawnResult
  -- | Clean up an agent (close tab, delete worktree).
  CleanupAgent :: Text -> Bool -> AgentControl m (Either Text ())
  -- | Clean up multiple agents.
  CleanupAgents :: [Text] -> Bool -> AgentControl m BatchCleanupResult
  -- | List all active agent worktrees.
  ListAgents :: AgentControl m (Either Text [AgentInfo])

-- Smart constructors (manually written - makeSem doesn't work with WASM cross-compilation)
spawnAgent :: Member AgentControl r => Text -> SpawnOptions -> Sem r (Either Text SpawnResult)
spawnAgent issueId opts = send (SpawnAgent issueId opts)

spawnAgents :: Member AgentControl r => [Text] -> SpawnOptions -> Sem r BatchSpawnResult
spawnAgents issueIds opts = send (SpawnAgents issueIds opts)

cleanupAgent :: Member AgentControl r => Text -> Bool -> Sem r (Either Text ())
cleanupAgent issueId force = send (CleanupAgent issueId force)

cleanupAgents :: Member AgentControl r => [Text] -> Bool -> Sem r BatchCleanupResult
cleanupAgents issueIds force = send (CleanupAgents issueIds force)

listAgents :: Member AgentControl r => Sem r (Either Text [AgentInfo])
listAgents = send ListAgents

-- ============================================================================
-- Interpreter
-- ============================================================================

-- | Parse an issue ID from text to Word64.
parseIssueId :: Text -> Either Text Word64
parseIssueId t = case TR.decimal t of
  Right (n, "") -> Right n
  _ -> Left $ "Invalid issue ID: " <> t

-- | Interpret AgentControl by calling Rust host functions.
runAgentControl :: (Member (Embed IO) r) => Sem (AgentControl ': r) a -> Sem r a
runAgentControl = interpret $ \case
  SpawnAgent issueId opts -> embed $ do
    case parseIssueId issueId of
      Left err -> pure $ Left err
      Right issueIdNum -> do
        let input =
              SpawnAgentInput
                { saiIssueId = issueIdNum,
                  saiOwner = owner opts,
                  saiRepo = repo opts,
                  saiWorktreeDir = worktreeDir opts,
                  saiAgentType = fromMaybe Gemini (agentType opts)
                }
        res <- callHost host_agent_spawn input
        pure $ case res of
          Left err -> Left (Data.Text.pack err)
          Right r -> Right r
  SpawnAgents issueIds opts -> embed $ do
    let input =
          SpawnAgentsInput
            { sasIssueIds = issueIds,
              sasOwner = owner opts,
              sasRepo = repo opts,
              sasWorktreeDir = worktreeDir opts,
              sasAgentType = fromMaybe Gemini (agentType opts)
            }
    res <- callHost host_agent_spawn_batch input
    pure $ case res of
      Left err ->
        BatchSpawnResult
          { spawned = [],
            spawnFailed = [("", Data.Text.pack err)]
          }
      Right r -> r
  CleanupAgent issueId force -> embed $ do
    let input = CleanupAgentInput issueId force
    res <- callHost host_agent_cleanup input
    pure $ case res of
      Left err -> Left (Data.Text.pack err)
      Right () -> Right ()
  CleanupAgents issueIds force -> embed $ do
    let input = CleanupAgentsInput issueIds force
    res <- callHost host_agent_cleanup_batch input
    pure $ case res of
      Left err ->
        BatchCleanupResult
          { cleaned = [],
            cleanupFailed = [("", Data.Text.pack err)]
          }
      Right r -> r
  ListAgents -> embed $ do
    res <- callHost host_agent_list ListAgentsInput
    pure $ case res of
      Left err -> Left (Data.Text.pack err)
      Right agents -> Right agents

