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
    SpawnOptions (..),
    SpawnResult (..),
    AgentInfo (..),
    BatchSpawnResult (..),
    BatchCleanupResult (..),
    HostResult (..),
  )
where

import Control.Monad.Freer
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text
import ExoMonad.Guest.HostCall (callHost, host_agent_cleanup, host_agent_cleanup_batch, host_agent_list, host_agent_spawn, host_agent_spawn_batch)
import GHC.Generics (Generic)

-- ============================================================================
-- Types (match Rust agent_control.rs)
-- ============================================================================

-- | Options for spawning an agent.
data SpawnOptions = SpawnOptions
  { owner :: Text,
    repo :: Text,
    worktreeDir :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON SpawnOptions where
  toJSON (SpawnOptions o r w) =
    object
      [ "owner" .= o,
        "repo" .= r,
        "worktree_dir" .= w
      ]

-- | Result of spawning an agent.
data SpawnResult = SpawnResult
  { worktreePath :: Text,
    branchName :: Text,
    tabName :: Text,
    issueTitle :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON SpawnResult where
  parseJSON = withObject "SpawnResult" $ \v ->
    SpawnResult
      <$> v .: "worktree_path"
      <*> v .: "branch_name"
      <*> v .: "tab_name"
      <*> v .: "issue_title"

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

-- | Host result wrapper (matches Rust HostResult).
data HostResult a
  = Success a
  | HostError Text
  deriving (Show, Eq, Generic)

instance (FromJSON a) => FromJSON (HostResult a) where
  parseJSON = withObject "HostResult" $ \v -> do
    kind <- v .: "kind" :: FromJSON (Maybe Text) => Data.Aeson.Types.Parser Text
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
  { saiIssueId :: Text,
    saiOwner :: Text,
    saiRepo :: Text,
    saiWorktreeDir :: Maybe Text
  }
  deriving (Show, Generic)

instance ToJSON SpawnAgentInput where
  toJSON (SpawnAgentInput i o r w) =
    object
      [ "issue_id" .= i,
        "owner" .= o,
        "repo" .= r,
        "worktree_dir" .= w
      ]

data SpawnAgentsInput = SpawnAgentsInput
  { sasIssueIds :: [Text],
    sasOwner :: Text,
    sasRepo :: Text,
    sasWorktreeDir :: Maybe Text
  }
  deriving (Show, Generic)

instance ToJSON SpawnAgentsInput where
  toJSON (SpawnAgentsInput is o r w) =
    object
      [ "issue_ids" .= is,
        "owner" .= o,
        "repo" .= r,
        "worktree_dir" .= w
      ]

data CleanupAgentInput = CleanupAgentInput
  { caiIssueId :: Text,
    caiForce :: Bool
  }
  deriving (Show, Generic)

instance ToJSON CleanupAgentInput where
  toJSON (CleanupAgentInput i f) =
    object
      [ "issue_id" .= i,
        "force" .= f
      ]

data CleanupAgentsInput = CleanupAgentsInput
  { casIssueIds :: [Text],
    casForce :: Bool
  }
  deriving (Show, Generic)

instance ToJSON CleanupAgentsInput where
  toJSON (CleanupAgentsInput is f) =
    object
      [ "issue_ids" .= is,
        "force" .= f
      ]

data ListAgentsInput = ListAgentsInput
  deriving (Show, Generic)

instance ToJSON ListAgentsInput where
  toJSON ListAgentsInput = object []

-- ============================================================================
-- Effect type
-- ============================================================================

-- | Agent control effect for high-level agent lifecycle management.
data AgentControl r where
  -- | Spawn an agent for a GitHub issue.
  SpawnAgent :: Text -> SpawnOptions -> AgentControl (Either Text SpawnResult)
  -- | Spawn multiple agents.
  SpawnAgents :: [Text] -> SpawnOptions -> AgentControl BatchSpawnResult
  -- | Clean up an agent (close tab, delete worktree).
  CleanupAgent :: Text -> Bool -> AgentControl (Either Text ())
  -- | Clean up multiple agents.
  CleanupAgents :: [Text] -> Bool -> AgentControl BatchCleanupResult
  -- | List all active agent worktrees.
  ListAgents :: AgentControl (Either Text [AgentInfo])

-- ============================================================================
-- Smart constructors
-- ============================================================================

spawnAgent :: (Member AgentControl effs) => Text -> SpawnOptions -> Eff effs (Either Text SpawnResult)
spawnAgent issueId opts = send (SpawnAgent issueId opts)

spawnAgents :: (Member AgentControl effs) => [Text] -> SpawnOptions -> Eff effs BatchSpawnResult
spawnAgents issueIds opts = send (SpawnAgents issueIds opts)

cleanupAgent :: (Member AgentControl effs) => Text -> Bool -> Eff effs (Either Text ())
cleanupAgent issueId force = send (CleanupAgent issueId force)

cleanupAgents :: (Member AgentControl effs) => [Text] -> Bool -> Eff effs BatchCleanupResult
cleanupAgents issueIds force = send (CleanupAgents issueIds force)

listAgents :: (Member AgentControl effs) => Eff effs (Either Text [AgentInfo])
listAgents = send ListAgents

-- ============================================================================
-- Interpreter
-- ============================================================================

-- | Interpret AgentControl by calling Rust host functions.
runAgentControl :: (LastMember IO effs) => Eff (AgentControl ': effs) a -> Eff effs a
runAgentControl = interpret $ \case
  SpawnAgent issueId opts -> sendM $ do
    let input =
          SpawnAgentInput
            { saiIssueId = issueId,
              saiOwner = owner opts,
              saiRepo = repo opts,
              saiWorktreeDir = worktreeDir opts
            }
    res <- callHost host_agent_spawn input
    pure $ case res of
      Left err -> Left (Data.Text.pack err)
      Right (Success r) -> Right r
      Right (HostError msg) -> Left msg
  SpawnAgents issueIds opts -> sendM $ do
    let input =
          SpawnAgentsInput
            { sasIssueIds = issueIds,
              sasOwner = owner opts,
              sasRepo = repo opts,
              sasWorktreeDir = worktreeDir opts
            }
    res <- callHost host_agent_spawn_batch input
    pure $ case res of
      Left err ->
        BatchSpawnResult
          { spawned = [],
            spawnFailed = [("", Data.Text.pack err)]
          }
      Right (Success r) -> r
      Right (HostError msg) ->
        BatchSpawnResult
          { spawned = [],
            spawnFailed = [("", msg)]
          }
  CleanupAgent issueId force -> sendM $ do
    let input = CleanupAgentInput issueId force
    res <- callHost host_agent_cleanup input
    pure $ case res of
      Left err -> Left (Data.Text.pack err)
      Right (Success ()) -> Right ()
      Right (HostError msg) -> Left msg
  CleanupAgents issueIds force -> sendM $ do
    let input = CleanupAgentsInput issueIds force
    res <- callHost host_agent_cleanup_batch input
    pure $ case res of
      Left err ->
        BatchCleanupResult
          { cleaned = [],
            cleanupFailed = [("", Data.Text.pack err)]
          }
      Right (Success r) -> r
      Right (HostError msg) ->
        BatchCleanupResult
          { cleaned = [],
            cleanupFailed = [("", msg)]
          }
  ListAgents -> sendM $ do
    res <- callHost host_agent_list ListAgentsInput
    pure $ case res of
      Left err -> Left (Data.Text.pack err)
      Right (Success agents) -> Right agents
      Right (HostError msg) -> Left msg
