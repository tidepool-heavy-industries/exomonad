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
    spawnWorker,

    -- * Interpreter
    runAgentControl,

    -- * Types
    AgentType (..),
    SpawnResult (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, withText, (.=), (.:))
import Data.Text (Text)
import Data.Text qualified as T
import Effects.Agent qualified as PA
import ExoMonad.Effects.Agent qualified as Agent
import ExoMonad.Guest.Proto (fromText, toText)
import GHC.Generics (Generic)
import Polysemy (Member, Sem, embed, interpret, send)
import Polysemy.Embed (Embed)
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

-- | Agent control effect for spawning agents.
data AgentControl m a where
  SpawnSubtreeC :: Text -> Text -> Maybe Text -> AgentType -> AgentControl m (Either Text SpawnResult)
  SpawnWorkerC :: Text -> Text -> AgentControl m (Either Text SpawnResult)

-- Smart constructors (manually written - makeSem doesn't work with WASM cross-compilation)
spawnSubtree :: (Member AgentControl r) => Text -> Text -> Maybe Text -> AgentType -> Sem r (Either Text SpawnResult)
spawnSubtree task branchName context agentTy = send (SpawnSubtreeC task branchName context agentTy)

spawnWorker :: (Member AgentControl r) => Text -> Text -> Sem r (Either Text SpawnResult)
spawnWorker name prompt = send (SpawnWorkerC name prompt)

-- ============================================================================
-- Interpreter (uses yield_effect via Effect typeclass)
-- ============================================================================

-- | Interpret AgentControl by calling Rust host via yield_effect.
runAgentControl :: (Member (Embed IO) r) => Sem (AgentControl ': r) a -> Sem r a
runAgentControl = interpret $ \case
  SpawnSubtreeC task branchName context agentTy -> embed $ do
    let req =
          PA.SpawnGeminiTeammateRequest
            { PA.spawnGeminiTeammateRequestName = fromText branchName,
              PA.spawnGeminiTeammateRequestPrompt = fromText (maybe task (\c -> task <> "\n\n" <> c) context),
              PA.spawnGeminiTeammateRequestAgentType = Enumerated (Right (toProtoAgentType agentTy)),
              PA.spawnGeminiTeammateRequestSubrepo = "",
              PA.spawnGeminiTeammateRequestTopology = Enumerated (Right PA.WorkspaceTopologyWORKSPACE_TOPOLOGY_WORKTREE_PER_AGENT),
              PA.spawnGeminiTeammateRequestBaseBranch = ""
            }
    result <- Agent.spawnGeminiTeammate req
    pure $ case result of
      Left err -> Left (T.pack (show err))
      Right resp -> case PA.spawnGeminiTeammateResponseAgent resp of
        Nothing -> Left "SpawnSubtree succeeded but no agent info returned"
        Just info -> Right (protoAgentInfoToSpawnResult info)
  SpawnWorkerC name prompt -> embed $ do
    let req =
          PA.SpawnWorkerRequest
            { PA.spawnWorkerRequestName = fromText name,
              PA.spawnWorkerRequestPrompt = fromText prompt
            }
    result <- Agent.spawnWorker req
    pure $ case result of
      Left err -> Left (T.pack (show err))
      Right resp -> case PA.spawnWorkerResponseAgent resp of
        Nothing -> Left "SpawnWorker succeeded but no agent info returned"
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
