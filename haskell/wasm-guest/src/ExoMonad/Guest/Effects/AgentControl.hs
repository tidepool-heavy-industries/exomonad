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
    spawnSubtree,
    spawnLeafSubtree,
    spawnWorker,
    spawnAcp,

    -- * Interpreters
    runAgentControlSuspend,

    -- * Types
    AgentType (..),
    SpawnResult (..),
    PermissionFlags (..),
    defaultPermFlags,
    SpawnSubtreeConfig (..),
    SpawnLeafSubtreeConfig (..),
    SpawnWorkerConfig (..),
    SpawnAcpConfig (..),
  )
where

import Control.Monad.Freer (Eff, Member, interpret, send)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, withText, (.:), (.=))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Effects.Agent qualified as PA
import ExoMonad.Effects.Agent qualified as Agent
import ExoMonad.Guest.Proto (fromText, toText)
import ExoMonad.Guest.Tool.Suspend.Types (SuspendYield)
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect)
import ExoMonad.Guest.Types.Permissions
import GHC.Generics (Generic)
import Proto3.Suite.Types (Enumerated (..))

-- ============================================================================
-- Types
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

-- ============================================================================
-- Effect type
-- ============================================================================

-- | Permission flags for spawned agents.
data PermissionFlags = PermissionFlags
  { permMode :: Maybe Text,
    allowedTools :: [Text],
    disallowedTools :: [Text]
  }
  deriving (Show, Eq, Generic)

-- | Default permission flags (no restrictions, backwards compat).
defaultPermFlags :: PermissionFlags
defaultPermFlags = PermissionFlags Nothing [] []

-- | Configuration for spawning a Claude subtree agent.
data SpawnSubtreeConfig = SpawnSubtreeConfig
  { stcTask :: Text,
    stcBranchName :: Text,
    stcForkSession :: Bool,
    stcRole :: Maybe Text,
    stcAgentType :: Maybe AgentType,
    stcPerms :: PermissionFlags,
    stcWorkingDir :: Maybe Text,
    stcPermissions :: Maybe ClaudePermissions,
    stcStandaloneRepo :: Bool,
    stcAllowedDirs :: [Text]
  }
  deriving (Show, Eq, Generic)

-- | Configuration for spawning a Gemini leaf subtree agent.
data SpawnLeafSubtreeConfig = SpawnLeafSubtreeConfig
  { slcTask :: Text,
    slcBranchName :: Text,
    slcRole :: Maybe Text,
    slcAgentType :: Maybe AgentType,
    slcPerms :: PermissionFlags,
    slcStandaloneRepo :: Bool,
    slcAllowedDirs :: [Text]
  }
  deriving (Show, Eq, Generic)

-- | Configuration for spawning a Gemini worker agent.
data SpawnWorkerConfig = SpawnWorkerConfig
  { swcName :: Text,
    swcPrompt :: Text,
    swcPerms :: PermissionFlags
  } deriving (Show, Eq, Generic)

-- | Configuration for spawning an ACP agent.
data SpawnAcpConfig = SpawnAcpConfig
  { sacName :: Text,
    sacPrompt :: Text,
    sacPerms :: PermissionFlags
  } deriving (Show, Eq, Generic)

-- | Agent control effect for spawning agents.
data AgentControl a where
  SpawnSubtreeC :: SpawnSubtreeConfig -> AgentControl (Either Text SpawnResult)
  SpawnLeafSubtreeC :: SpawnLeafSubtreeConfig -> AgentControl (Either Text SpawnResult)
  SpawnWorkerC :: SpawnWorkerConfig -> AgentControl (Either Text SpawnResult)
  SpawnAcpC :: SpawnAcpConfig -> AgentControl (Either Text SpawnResult)

-- Smart constructors (manually written - makeSem doesn't work with WASM cross-compilation)
spawnSubtree :: (Member AgentControl r) => SpawnSubtreeConfig -> Eff r (Either Text SpawnResult)
spawnSubtree cfg = send (SpawnSubtreeC cfg)

spawnLeafSubtree :: (Member AgentControl r) => SpawnLeafSubtreeConfig -> Eff r (Either Text SpawnResult)
spawnLeafSubtree cfg = send (SpawnLeafSubtreeC cfg)

spawnWorker :: (Member AgentControl r) => SpawnWorkerConfig -> Eff r (Either Text SpawnResult)
spawnWorker cfg = send (SpawnWorkerC cfg)

spawnAcp :: (Member AgentControl r) => SpawnAcpConfig -> Eff r (Either Text SpawnResult)
spawnAcp cfg = send (SpawnAcpC cfg)

-- ============================================================================
-- Interpreter (uses yield_effect via Effect typeclass)
-- ============================================================================

-- | Interpret AgentControl via coroutine suspend (trampoline path).
-- Effects dispatched async without holding the WASM plugin lock.
runAgentControlSuspend :: (Member SuspendYield r) => Eff (AgentControl ': r) a -> Eff r a
runAgentControlSuspend = interpret $ \case
  SpawnSubtreeC cfg -> do
    let req =
          PA.SpawnSubtreeRequest
            { PA.spawnSubtreeRequestTask = fromText (stcTask cfg),
              PA.spawnSubtreeRequestBranchName = fromText (stcBranchName cfg),
              PA.spawnSubtreeRequestParentSessionId = fromText "",
              PA.spawnSubtreeRequestForkSession = stcForkSession cfg,
              PA.spawnSubtreeRequestRole = fromText (fromMaybe "" (stcRole cfg)),
              PA.spawnSubtreeRequestAgentType = Enumerated (Right (maybe PA.AgentTypeAGENT_TYPE_UNSPECIFIED toProtoAgentType (stcAgentType cfg))),
              PA.spawnSubtreeRequestPermissionMode = fromText (fromMaybe "" (permMode (stcPerms cfg))),
              PA.spawnSubtreeRequestAllowedTools = V.fromList (map fromText (allowedTools (stcPerms cfg))),
              PA.spawnSubtreeRequestDisallowedTools = V.fromList (map fromText (disallowedTools (stcPerms cfg))),
              PA.spawnSubtreeRequestWorkingDir = fromText (fromMaybe "" (stcWorkingDir cfg)),
              PA.spawnSubtreeRequestPermissions = fmap permissionsToProto (stcPermissions cfg),
              PA.spawnSubtreeRequestStandaloneRepo = stcStandaloneRepo cfg,
              PA.spawnSubtreeRequestAllowedDirs = V.fromList (map fromText (stcAllowedDirs cfg))
            }
    result <- suspendEffect @Agent.AgentSpawnSubtree req
    pure $ case result of
      Left err -> Left (T.pack (show err))
      Right resp -> case PA.spawnSubtreeResponseAgent resp of
        Nothing -> Left "SpawnSubtree succeeded but no agent info returned"
        Just info -> Right (protoAgentInfoToSpawnResult info)
  SpawnLeafSubtreeC cfg -> do
    let req =
          PA.SpawnLeafSubtreeRequest
            { PA.spawnLeafSubtreeRequestTask = fromText (slcTask cfg),
              PA.spawnLeafSubtreeRequestBranchName = fromText (slcBranchName cfg),
              PA.spawnLeafSubtreeRequestRole = fromText (fromMaybe "" (slcRole cfg)),
              PA.spawnLeafSubtreeRequestAgentType = Enumerated (Right (maybe PA.AgentTypeAGENT_TYPE_UNSPECIFIED toProtoAgentType (slcAgentType cfg))),
              PA.spawnLeafSubtreeRequestPermissionMode = fromText (fromMaybe "" (permMode (slcPerms cfg))),
              PA.spawnLeafSubtreeRequestAllowedTools = V.fromList (map fromText (allowedTools (slcPerms cfg))),
              PA.spawnLeafSubtreeRequestDisallowedTools = V.fromList (map fromText (disallowedTools (slcPerms cfg))),
              PA.spawnLeafSubtreeRequestStandaloneRepo = slcStandaloneRepo cfg,
              PA.spawnLeafSubtreeRequestAllowedDirs = V.fromList (map fromText (slcAllowedDirs cfg))
            }
    result <- suspendEffect @Agent.AgentSpawnLeafSubtree req
    pure $ case result of
      Left err -> Left (T.pack (show err))
      Right resp -> case PA.spawnLeafSubtreeResponseAgent resp of
        Nothing -> Left "SpawnLeafSubtree succeeded but no agent info returned"
        Just info -> Right (protoAgentInfoToSpawnResult info)
  SpawnWorkerC cfg -> do
    let req =
          PA.SpawnWorkerRequest
            { PA.spawnWorkerRequestName = fromText (swcName cfg),
              PA.spawnWorkerRequestPrompt = fromText (swcPrompt cfg),
              PA.spawnWorkerRequestPermissionMode = fromText (fromMaybe "" (permMode (swcPerms cfg))),
              PA.spawnWorkerRequestAllowedTools = V.fromList (map fromText (allowedTools (swcPerms cfg))),
              PA.spawnWorkerRequestDisallowedTools = V.fromList (map fromText (disallowedTools (swcPerms cfg)))
            }
    result <- suspendEffect @Agent.AgentSpawnWorker req
    pure $ case result of
      Left err -> Left (T.pack (show err))
      Right resp -> case PA.spawnWorkerResponseAgent resp of
        Nothing -> Left "SpawnWorker succeeded but no agent info returned"
        Just info -> Right (protoAgentInfoToSpawnResult info)
  SpawnAcpC cfg -> do
    let req =
          PA.SpawnAcpRequest
            { PA.spawnAcpRequestName = fromText (sacName cfg),
              PA.spawnAcpRequestPrompt = fromText (sacPrompt cfg),
              PA.spawnAcpRequestPermissionMode = fromText (fromMaybe "" (permMode (sacPerms cfg))),
              PA.spawnAcpRequestAllowedTools = V.fromList (map fromText (allowedTools (sacPerms cfg))),
              PA.spawnAcpRequestDisallowedTools = V.fromList (map fromText (disallowedTools (sacPerms cfg)))
            }
    result <- suspendEffect @Agent.AgentSpawnAcp req
    pure $ case result of
      Left err -> Left (T.pack (show err))
      Right resp -> case PA.spawnAcpResponseAgent resp of
        Nothing -> Left "SpawnAcp succeeded but no agent info returned"
        Just info -> Right (protoAgentInfoToSpawnResult info)

-- ============================================================================
-- Conversion helpers
-- ============================================================================

toProtoAgentType :: AgentType -> PA.AgentType
toProtoAgentType Claude = PA.AgentTypeAGENT_TYPE_CLAUDE
toProtoAgentType Gemini = PA.AgentTypeAGENT_TYPE_GEMINI

permissionsToProto :: ClaudePermissions -> PA.Permissions
permissionsToProto perms =
  PA.Permissions
    { PA.permissionsAllow = V.fromList (map (fromText . renderToolPattern) (cpAllow perms)),
      PA.permissionsDeny = V.fromList (map (fromText . renderToolPattern) (cpDeny perms))
    }

protoAgentInfoToSpawnResult :: PA.AgentInfo -> SpawnResult
protoAgentInfoToSpawnResult info =
  SpawnResult
    { worktreePath = toText (PA.agentInfoWorktreePath info),
      branchName = toText (PA.agentInfoBranchName info),
      tabName = toText (PA.agentInfoZellijTab info),
      issueTitle = toText (PA.agentInfoIssue info),
      agentTypeResult = case PA.agentInfoAgentType info of
        Enumerated (Right PA.AgentTypeAGENT_TYPE_CLAUDE) -> "claude"
        Enumerated (Right PA.AgentTypeAGENT_TYPE_GEMINI) -> "gemini"
        _ -> "unknown"
    }
