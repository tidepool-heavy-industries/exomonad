{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | High-level agent control effects.
--
-- These effects provide semantic operations for agent lifecycle management.
-- The Rust host handles all I/O (git, zellij, filesystem) via yield_effect.
module ExoMonad.Guest.Effects.AgentControl
  ( -- * Effect type
    AgentControl (..),

    -- * Smart constructors
    spawnAgent,
    spawnAgents,
    spawnGeminiTeammate,
    cleanupAgent,
    cleanupAgents,
    cleanupMergedAgents,
    listAgents,

    -- * Interpreter
    runAgentControl,

    -- * Types
    AgentType (..),
    SpawnOptions (..),
    SpawnResult (..),
    AgentPrInfo (..),
    AgentInfo (..),
    BatchSpawnResult (..),
    BatchCleanupResult (..),
    AgentStatus (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, withText, (.:), (.:?), (.=))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Vector qualified as V
import Effects.Agent qualified as PA
import ExoMonad.Common (Role (..))
import ExoMonad.Effects.Agent qualified as Agent
import GHC.Generics (Generic)
import Proto3.Suite.Types (Enumerated (..))
import Polysemy (Member, Sem, embed, interpret, send)
import Polysemy.Embed (Embed)

-- ============================================================================
-- Types (maintained for backward compatibility with callers)
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
    other -> fail $ "Invalid agent type: " <> T.unpack other

-- | Options for spawning an agent.
data SpawnOptions = SpawnOptions
  { owner :: Text,
    repo :: Text,
    worktreeDir :: Maybe Text,
    agentType :: AgentType,
    subrepo :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON SpawnOptions where
  toJSON (SpawnOptions o r w a s) =
    object
      [ "owner" .= o,
        "repo" .= r,
        "worktree_dir" .= w,
        "agent_type" .= a,
        "subrepo" .= s
      ]

instance FromJSON SpawnOptions where
  parseJSON = withObject "SpawnOptions" $ \v ->
    SpawnOptions
      <$> v .: "owner"
      <*> v .: "repo"
      <*> v .: "worktree_dir"
      <*> v .: "agent_type"
      <*> v .:? "subrepo"

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

-- | Simplified PR info for agent listing.
data AgentPrInfo = AgentPrInfo
  { prNumber :: Int,
    prTitle :: Text,
    prUrl :: Text,
    prState :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON AgentPrInfo where
  parseJSON = withObject "AgentPrInfo" $ \v ->
    AgentPrInfo
      <$> v .: "number"
      <*> v .: "title"
      <*> v .: "url"
      <*> v .: "state"

instance ToJSON AgentPrInfo where
  toJSON (AgentPrInfo n t u s) =
    object
      [ "number" .= n,
        "title" .= t,
        "url" .= u,
        "state" .= s
      ]

data AgentStatus = RUNNING | ORPHAN_WORKTREE | ORPHAN_TAB
  deriving (Show, Eq, Generic)

instance FromJSON AgentStatus
instance ToJSON AgentStatus

-- | Information about an active agent.
data AgentInfo = AgentInfo
  { agentIssueId :: Text,
    agentHasTab :: Bool,
    agentHasWorktree :: Bool,
    agentHasChanges :: Maybe Bool,
    agentHasUnpushed :: Maybe Bool,
    agentStatus :: AgentStatus,
    agentWorktreePath :: Maybe Text,
    agentBranchName :: Maybe Text,
    agentSlug :: Maybe Text,
    agentAgentType :: Maybe Text,
    agentPr :: Maybe AgentPrInfo
  }
  deriving (Show, Eq, Generic)

instance FromJSON AgentInfo where
  parseJSON = withObject "AgentInfo" $ \v ->
    AgentInfo
      <$> v .: "issue_id"
      <*> v .: "has_tab"
      <*> v .: "has_worktree"
      <*> v .:? "has_changes"
      <*> v .:? "has_unpushed"
      <*> v .: "status"
      <*> v .:? "worktree_path"
      <*> v .:? "branch_name"
      <*> v .:? "slug"
      <*> v .:? "agent_type"
      <*> v .:? "pr"

instance ToJSON AgentInfo where
  toJSON (AgentInfo i ht hw hc hu st w b s at pr) =
    object $
      [ "issue_id" .= i,
        "has_tab" .= ht,
        "has_worktree" .= hw,
        "status" .= st
      ]
        ++ catMaybes
          [ ("has_changes" .=) <$> hc,
            ("has_unpushed" .=) <$> hu,
            ("worktree_path" .=) <$> w,
            ("branch_name" .=) <$> b,
            ("slug" .=) <$> s,
            ("agent_type" .=) <$> at,
            ("pr" .=) <$> pr
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

-- ============================================================================
-- Effect type
-- ============================================================================

-- | Agent control effect for high-level agent lifecycle management.
data AgentControl m a where
  SpawnAgent :: Text -> SpawnOptions -> AgentControl m (Either Text SpawnResult)
  SpawnAgents :: [Text] -> SpawnOptions -> AgentControl m BatchSpawnResult
  SpawnGeminiTeammate :: Text -> Text -> AgentType -> Maybe Text -> Maybe Text -> AgentControl m (Either Text SpawnResult)
  CleanupAgent :: Text -> Bool -> Maybe Text -> AgentControl m (Either Text ())
  CleanupAgents :: [Text] -> Bool -> Maybe Text -> AgentControl m BatchCleanupResult
  CleanupMergedAgents :: Maybe Text -> AgentControl m BatchCleanupResult
  ListAgents :: Maybe Text -> AgentControl m (Either Text [AgentInfo])

-- Smart constructors (manually written - makeSem doesn't work with WASM cross-compilation)
spawnAgent :: (Member AgentControl r) => Text -> SpawnOptions -> Sem r (Either Text SpawnResult)
spawnAgent issueId opts = send (SpawnAgent issueId opts)

spawnAgents :: (Member AgentControl r) => [Text] -> SpawnOptions -> Sem r BatchSpawnResult
spawnAgents issueIds opts = send (SpawnAgents issueIds opts)

spawnGeminiTeammate :: (Member AgentControl r) => Text -> Text -> AgentType -> Maybe Text -> Maybe Text -> Sem r (Either Text SpawnResult)
spawnGeminiTeammate name prompt agentTy subrepo teamName = send (SpawnGeminiTeammate name prompt agentTy subrepo teamName)

cleanupAgent :: (Member AgentControl r) => Text -> Bool -> Maybe Text -> Sem r (Either Text ())
cleanupAgent issueId force subrepo = send (CleanupAgent issueId force subrepo)

cleanupAgents :: (Member AgentControl r) => [Text] -> Bool -> Maybe Text -> Sem r BatchCleanupResult
cleanupAgents issueIds force subrepo = send (CleanupAgents issueIds force subrepo)

cleanupMergedAgents :: (Member AgentControl r) => Maybe Text -> Sem r BatchCleanupResult
cleanupMergedAgents subrepo = send (CleanupMergedAgents subrepo)

listAgents :: (Member AgentControl r) => Maybe Text -> Sem r (Either Text [AgentInfo])
listAgents subrepo = send (ListAgents subrepo)

-- ============================================================================
-- Interpreter (uses yield_effect via Effect typeclass)
-- ============================================================================

-- | Interpret AgentControl by calling Rust host via yield_effect.
runAgentControl :: (Member (Embed IO) r) => Sem (AgentControl ': r) a -> Sem r a
runAgentControl = interpret $ \case
  SpawnAgent issueId opts -> embed $ do
    let req =
          PA.SpawnRequest
            { PA.spawnRequestIssue = TL.fromStrict issueId,
              PA.spawnRequestOwner = TL.fromStrict (owner opts),
              PA.spawnRequestRepo = TL.fromStrict (repo opts),
              PA.spawnRequestAgentType = Enumerated (Right (toProtoAgentType (agentType opts))),
              PA.spawnRequestRole = Enumerated (Right RoleROLE_UNSPECIFIED),
              PA.spawnRequestWorktreeDir = maybe "" TL.fromStrict (worktreeDir opts),
              PA.spawnRequestSubrepo = maybe "" TL.fromStrict (subrepo opts),
              PA.spawnRequestTopology = Enumerated (Right PA.WorkspaceTopologyWORKSPACE_TOPOLOGY_WORKTREE_PER_AGENT)
            }
    result <- Agent.spawnAgent req
    pure $ case result of
      Left err -> Left (T.pack (show err))
      Right resp -> case PA.spawnResponseAgent resp of
        Nothing -> Left "Spawn succeeded but no agent info returned"
        Just info -> Right (protoAgentInfoToSpawnResult info)
  SpawnAgents issueIds opts -> embed $ do
    let req =
          PA.SpawnBatchRequest
            { PA.spawnBatchRequestIssues = V.fromList (map TL.fromStrict issueIds),
              PA.spawnBatchRequestOwner = TL.fromStrict (owner opts),
              PA.spawnBatchRequestRepo = TL.fromStrict (repo opts),
              PA.spawnBatchRequestAgentType = Enumerated (Right (toProtoAgentType (agentType opts))),
              PA.spawnBatchRequestRole = Enumerated (Right RoleROLE_UNSPECIFIED),
              PA.spawnBatchRequestWorktreeDir = maybe "" TL.fromStrict (worktreeDir opts),
              PA.spawnBatchRequestSubrepo = maybe "" TL.fromStrict (subrepo opts),
              PA.spawnBatchRequestTopology = Enumerated (Right PA.WorkspaceTopologyWORKSPACE_TOPOLOGY_WORKTREE_PER_AGENT)
            }
    result <- Agent.spawnBatch req
    pure $ case result of
      Left err ->
        BatchSpawnResult
          { spawned = [],
            spawnFailed = [("", T.pack (show err))]
          }
      Right resp ->
        BatchSpawnResult
          { spawned = map protoAgentInfoToSpawnResult (V.toList (PA.spawnBatchResponseAgents resp)),
            spawnFailed = map (\e -> ("", TL.toStrict e)) (V.toList (PA.spawnBatchResponseErrors resp))
          }
  SpawnGeminiTeammate name prompt agentTy subrepo teamName -> embed $ do
    let req =
          PA.SpawnGeminiTeammateRequest
            { PA.spawnGeminiTeammateRequestName = TL.fromStrict name,
              PA.spawnGeminiTeammateRequestPrompt = TL.fromStrict prompt,
              PA.spawnGeminiTeammateRequestAgentType = Enumerated (Right (toProtoAgentType agentTy)),
              PA.spawnGeminiTeammateRequestSubrepo = maybe "" TL.fromStrict subrepo,
              PA.spawnGeminiTeammateRequestTeamName = maybe "" TL.fromStrict teamName,
              PA.spawnGeminiTeammateRequestTopology = Enumerated (Right PA.WorkspaceTopologyWORKSPACE_TOPOLOGY_SHARED_DIR)
            }
    result <- Agent.spawnGeminiTeammate req
    pure $ case result of
      Left err -> Left (T.pack (show err))
      Right resp -> case PA.spawnGeminiTeammateResponseAgent resp of
        Nothing -> Left "SpawnGeminiTeammate succeeded but no agent info returned"
        Just info -> Right (protoAgentInfoToSpawnResult info)
  CleanupAgent issueId force subrepo -> embed $ do
    let req =
          PA.CleanupRequest
            { PA.cleanupRequestIssue = TL.fromStrict issueId,
              PA.cleanupRequestForce = force,
              PA.cleanupRequestSubrepo = maybe "" TL.fromStrict subrepo
            }
    result <- Agent.cleanupAgent req
    pure $ case result of
      Left err -> Left (T.pack (show err))
      Right resp ->
        if PA.cleanupResponseSuccess resp
          then Right ()
          else Left (TL.toStrict (PA.cleanupResponseError resp))
  CleanupAgents issueIds force subrepo -> embed $ do
    let req =
          PA.CleanupBatchRequest
            { PA.cleanupBatchRequestIssues = V.fromList (map TL.fromStrict issueIds),
              PA.cleanupBatchRequestForce = force,
              PA.cleanupBatchRequestSubrepo = maybe "" TL.fromStrict subrepo
            }
    result <- Agent.cleanupBatch req
    pure $ case result of
      Left err ->
        BatchCleanupResult
          { cleaned = [],
            cleanupFailed = [("", T.pack (show err))]
          }
      Right resp ->
        BatchCleanupResult
          { cleaned = map TL.toStrict (V.toList (PA.cleanupBatchResponseCleaned resp)),
            cleanupFailed = zip
              (map TL.toStrict (V.toList (PA.cleanupBatchResponseFailed resp)))
              (map TL.toStrict (V.toList (PA.cleanupBatchResponseErrors resp)))
          }
  CleanupMergedAgents subrepo -> embed $ do
    let req =
          PA.CleanupMergedRequest
            { PA.cleanupMergedRequestIssues = V.empty,
              PA.cleanupMergedRequestSubrepo = maybe "" TL.fromStrict subrepo
            }
    result <- Agent.cleanupMerged req
    pure $ case result of
      Left err ->
        BatchCleanupResult
          { cleaned = [],
            cleanupFailed = [("", T.pack (show err))]
          }
      Right resp ->
        BatchCleanupResult
          { cleaned = map TL.toStrict (V.toList (PA.cleanupMergedResponseCleaned resp)),
            cleanupFailed = zip
              (map TL.toStrict (V.toList (PA.cleanupMergedResponseSkipped resp)))
              (map TL.toStrict (V.toList (PA.cleanupMergedResponseErrors resp)))
          }
  ListAgents subrepo -> embed $ do
    let req =
          PA.ListRequest
            { PA.listRequestFilterStatus = Enumerated (Right PA.AgentStatusAGENT_STATUS_UNSPECIFIED),
              PA.listRequestFilterRole = Enumerated (Right RoleROLE_UNSPECIFIED),
              PA.listRequestSubrepo = maybe "" TL.fromStrict subrepo
            }
    result <- Agent.listAgents req
    pure $ case result of
      Left err -> Left (T.pack (show err))
      Right resp -> Right (map protoAgentInfoToAgentInfo (V.toList (PA.listResponseAgents resp)))

-- ============================================================================
-- Conversion helpers
-- ============================================================================

toProtoAgentType :: AgentType -> PA.AgentType
toProtoAgentType Claude = PA.AgentTypeAGENT_TYPE_CLAUDE
toProtoAgentType Gemini = PA.AgentTypeAGENT_TYPE_GEMINI

protoAgentInfoToSpawnResult :: PA.AgentInfo -> SpawnResult
protoAgentInfoToSpawnResult info =
  SpawnResult
    { worktreePath = TL.toStrict (PA.agentInfoWorktreePath info),
      branchName = TL.toStrict (PA.agentInfoBranchName info),
      tabName = TL.toStrict (PA.agentInfoZellijTab info),
      issueTitle = TL.toStrict (PA.agentInfoIssue info),
      agentTypeResult = case PA.agentInfoAgentType info of
        Enumerated (Right PA.AgentTypeAGENT_TYPE_CLAUDE) -> "claude"
        Enumerated (Right PA.AgentTypeAGENT_TYPE_GEMINI) -> "gemini"
        _ -> "unknown"
    }

protoAgentInfoToAgentInfo :: PA.AgentInfo -> AgentInfo
protoAgentInfoToAgentInfo info =
  AgentInfo
    { agentIssueId = TL.toStrict (PA.agentInfoIssue info),
      agentHasTab = not (TL.null (PA.agentInfoZellijTab info)),
      agentHasWorktree = not (TL.null (PA.agentInfoWorktreePath info)),
      agentHasChanges = Nothing,
      agentHasUnpushed = Nothing,
      agentStatus = RUNNING,
      agentWorktreePath = let p = TL.toStrict (PA.agentInfoWorktreePath info) in if T.null p then Nothing else Just p,
      agentBranchName = let b = TL.toStrict (PA.agentInfoBranchName info) in if T.null b then Nothing else Just b,
      agentSlug = Nothing,
      agentAgentType = Just $ case PA.agentInfoAgentType info of
        Enumerated (Right PA.AgentTypeAGENT_TYPE_CLAUDE) -> "claude"
        Enumerated (Right PA.AgentTypeAGENT_TYPE_GEMINI) -> "gemini"
        _ -> "unknown",
      agentPr = let prNum = fromIntegral (PA.agentInfoPrNumber info)
                    prUrl = TL.toStrict (PA.agentInfoPrUrl info)
                 in if prNum == 0 then Nothing
                    else Just AgentPrInfo
                      { prNumber = prNum,
                        prTitle = "",
                        prUrl = prUrl,
                        prState = "open"
                      }
    }
