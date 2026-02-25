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
  )
where

import Control.Monad.Freer (Eff, Member, interpret, send)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, withText, (.:), (.=))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Effects.Agent qualified as PA
import ExoMonad.Effects.Agent qualified as Agent
import ExoMonad.Guest.Proto (fromText, toText)
import ExoMonad.Guest.Tool.Suspend.Types (SuspendYield)
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect)
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

-- | Agent control effect for spawning agents.
data AgentControl a where
  SpawnSubtreeC :: Text -> Text -> Text -> Bool -> Maybe Text -> Maybe AgentType -> PermissionFlags -> Maybe Bool -> Maybe [Text] -> AgentControl (Either Text SpawnResult)
  SpawnLeafSubtreeC :: Text -> Text -> Maybe Text -> Maybe AgentType -> PermissionFlags -> Maybe Bool -> Maybe [Text] -> AgentControl (Either Text SpawnResult)
  SpawnWorkerC :: Text -> Text -> PermissionFlags -> AgentControl (Either Text SpawnResult)
  SpawnAcpC :: Text -> Text -> PermissionFlags -> AgentControl (Either Text SpawnResult)

-- Smart constructors (manually written - makeSem doesn't work with WASM cross-compilation)
spawnSubtree :: (Member AgentControl r) => Text -> Text -> Text -> Bool -> Maybe Text -> Maybe AgentType -> PermissionFlags -> Maybe Bool -> Maybe [Text] -> Eff r (Either Text SpawnResult)
spawnSubtree task branchName parentSessionId forkSession role agentType perms secureMode allowedReadPaths = send (SpawnSubtreeC task branchName parentSessionId forkSession role agentType perms secureMode allowedReadPaths)

spawnLeafSubtree :: (Member AgentControl r) => Text -> Text -> Maybe Text -> Maybe AgentType -> PermissionFlags -> Maybe Bool -> Maybe [Text] -> Eff r (Either Text SpawnResult)
spawnLeafSubtree task branchName role agentType perms secureMode allowedReadPaths = send (SpawnLeafSubtreeC task branchName role agentType perms secureMode allowedReadPaths)

spawnWorker :: (Member AgentControl r) => Text -> Text -> PermissionFlags -> Eff r (Either Text SpawnResult)
spawnWorker name prompt perms = send (SpawnWorkerC name prompt perms)

spawnAcp :: (Member AgentControl r) => Text -> Text -> PermissionFlags -> Eff r (Either Text SpawnResult)
spawnAcp name prompt perms = send (SpawnAcpC name prompt perms)

-- ============================================================================
-- Interpreter (uses yield_effect via Effect typeclass)
-- ============================================================================

-- | Interpret AgentControl via coroutine suspend (trampoline path).
-- Effects dispatched async without holding the WASM plugin lock.
runAgentControlSuspend :: (Member SuspendYield r) => Eff (AgentControl ': r) a -> Eff r a
runAgentControlSuspend = interpret $ \case
  SpawnSubtreeC task branchName parentSessionId forkSession role agentType perms secureMode allowedReadPaths -> do
    let req =
          PA.SpawnSubtreeRequest
            { PA.spawnSubtreeRequestTask = fromText task,
              PA.spawnSubtreeRequestBranchName = fromText branchName,
              PA.spawnSubtreeRequestParentSessionId = fromText parentSessionId,
              PA.spawnSubtreeRequestForkSession = forkSession,
              PA.spawnSubtreeRequestRole = fromText (maybe "" id role),
              PA.spawnSubtreeRequestAgentType = Enumerated (Right (maybe PA.AgentTypeAGENT_TYPE_UNSPECIFIED toProtoAgentType agentType)),
              PA.spawnSubtreeRequestPermissionMode = fromText (maybe "" id (permMode perms)),
              PA.spawnSubtreeRequestAllowedTools = V.fromList (map fromText (allowedTools perms)),
              PA.spawnSubtreeRequestDisallowedTools = V.fromList (map fromText (disallowedTools perms)),
              PA.spawnSubtreeRequestSecureMode = maybe False id secureMode,
              PA.spawnSubtreeRequestAllowedReadPaths = V.fromList (map fromText (maybe [] id allowedReadPaths))
            }
    result <- suspendEffect @Agent.AgentSpawnSubtree req
    pure $ case result of
      Left err -> Left (T.pack (show err))
      Right resp -> case PA.spawnSubtreeResponseAgent resp of
        Nothing -> Left "SpawnSubtree succeeded but no agent info returned"
        Just info -> Right (protoAgentInfoToSpawnResult info)
  SpawnLeafSubtreeC task branchName role agentType perms secureMode allowedReadPaths -> do
    let req =
          PA.SpawnLeafSubtreeRequest
            { PA.spawnLeafSubtreeRequestTask = fromText task,
              PA.spawnLeafSubtreeRequestBranchName = fromText branchName,
              PA.spawnLeafSubtreeRequestRole = fromText (maybe "" id role),
              PA.spawnLeafSubtreeRequestAgentType = Enumerated (Right (maybe PA.AgentTypeAGENT_TYPE_UNSPECIFIED toProtoAgentType agentType)),
              PA.spawnLeafSubtreeRequestPermissionMode = fromText (maybe "" id (permMode perms)),
              PA.spawnLeafSubtreeRequestAllowedTools = V.fromList (map fromText (allowedTools perms)),
              PA.spawnLeafSubtreeRequestDisallowedTools = V.fromList (map fromText (disallowedTools perms)),
              PA.spawnLeafSubtreeRequestSecureMode = maybe False id secureMode,
              PA.spawnLeafSubtreeRequestAllowedReadPaths = V.fromList (map fromText (maybe [] id allowedReadPaths))
            }
    result <- suspendEffect @Agent.AgentSpawnLeafSubtree req
    pure $ case result of
      Left err -> Left (T.pack (show err))
      Right resp -> case PA.spawnLeafSubtreeResponseAgent resp of
        Nothing -> Left "SpawnLeafSubtree succeeded but no agent info returned"
        Just info -> Right (protoAgentInfoToSpawnResult info)
  SpawnWorkerC name prompt perms -> do
    let req =
          PA.SpawnWorkerRequest
            { PA.spawnWorkerRequestName = fromText name,
              PA.spawnWorkerRequestPrompt = fromText prompt,
              PA.spawnWorkerRequestPermissionMode = fromText (maybe "" id (permMode perms)),
              PA.spawnWorkerRequestAllowedTools = V.fromList (map fromText (allowedTools perms)),
              PA.spawnWorkerRequestDisallowedTools = V.fromList (map fromText (disallowedTools perms))
            }
    result <- suspendEffect @Agent.AgentSpawnWorker req
    pure $ case result of
      Left err -> Left (T.pack (show err))
      Right resp -> case PA.spawnWorkerResponseAgent resp of
        Nothing -> Left "SpawnWorker succeeded but no agent info returned"
        Just info -> Right (protoAgentInfoToSpawnResult info)
  SpawnAcpC name prompt perms -> do
    let req =
          PA.SpawnAcpRequest
            { PA.spawnAcpRequestName = fromText name,
              PA.spawnAcpRequestPrompt = fromText prompt,
              PA.spawnAcpRequestPermissionMode = fromText (maybe "" id (permMode perms)),
              PA.spawnAcpRequestAllowedTools = V.fromList (map fromText (allowedTools perms)),
              PA.spawnAcpRequestDisallowedTools = V.fromList (map fromText (disallowedTools perms))
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
