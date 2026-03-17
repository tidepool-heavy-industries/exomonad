{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Root TL role: orchestration-only. No file_pr, notify_parent, or shutdown.
--   Used for the root human-facing TL window (exomonad init).
module RootRole (config, Tools) where

import Control.Monad (void, forM_, when)
import Control.Monad.Freer (Eff)
import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import ExoMonad
import ExoMonad.Guest.StateMachine (applyEvent, StopCheckResult(..), checkExit)
import ExoMonad.Guest.Effects.StopHook (checkUncommittedWork, getCurrentBranch)
import ExoMonad.Guest.Tools.MergePR (mergePRCore, mergePRDescription, mergePRSchema, mergePRRender, MergePRArgs (..), MergePROutput (..), extractSlug)
import ExoMonad.Guest.Tools.Spawn
  ( forkWaveCore, forkWaveDescription, forkWaveSchema, forkWaveRender, ForkWaveArgs (..), ForkWaveResult (..),
    spawnLeafSubtreeCore, spawnLeafSubtreeDescription, spawnLeafSubtreeSchema, spawnLeafRender, SpawnLeafSubtreeArgs (..)
  )
import ExoMonad.Guest.Effects.AgentControl (SpawnResult (..))
import ExoMonad.Guest.Types (StopDecision(..), StopHookOutput(..), blockStopResponse, allowStopResponse, allowResponse)
import ExoMonad.Types (HookConfig (..), Effects, defaultSessionStartHook, teamRegistrationPostToolUse)
import PRReviewHandler (prReviewEventHandlers)
import TLPhase (TLPhase (..), TLEvent (..), ChildHandle (..))

data RootForkWave
instance MCPTool RootForkWave where
  type ToolArgs RootForkWave = ForkWaveArgs
  toolName = "fork_wave"
  toolDescription = forkWaveDescription
  toolSchema = forkWaveSchema
  toolHandlerEff args = do
    result <- forkWaveCore args
    case result of
      Left err -> pure $ errorResult err
      Right fwResult -> do
        forM_ (fwrSpawned fwResult) $ \(slug, sr) -> do
          let handle = ChildHandle { chSlug = slug, chBranch = branchName sr, chAgentType = agentTypeResult sr }
          void $ applyEvent @TLPhase @TLEvent TLPlanning (ChildSpawned handle)
        pure $ forkWaveRender fwResult

data RootSpawnGemini
instance MCPTool RootSpawnGemini where
  type ToolArgs RootSpawnGemini = SpawnLeafSubtreeArgs
  toolName = "spawn_gemini"
  toolDescription = spawnLeafSubtreeDescription
  toolSchema = spawnLeafSubtreeSchema
  toolHandlerEff args = do
    result <- spawnLeafSubtreeCore args
    case result of
      Left err -> pure $ errorResult err
      Right (slug, sr) -> do
        let handle = ChildHandle { chSlug = slug, chBranch = branchName sr, chAgentType = agentTypeResult sr }
        void $ applyEvent @TLPhase @TLEvent TLPlanning (ChildSpawned handle)
        pure $ spawnLeafRender result

data RootMergePR
instance MCPTool RootMergePR where
  type ToolArgs RootMergePR = MergePRArgs
  toolName = "merge_pr"
  toolDescription = mergePRDescription
  toolSchema = mergePRSchema
  toolHandlerEff args = do
    result <- mergePRCore args
    case result of
      Left err -> pure $ errorResult err
      Right output -> do
        when (mpoSuccess output) $ do
          case extractSlug (mpoBranchName output) of
            Just slug -> void $ applyEvent @TLPhase @TLEvent TLPlanning (ChildCompleted slug)
            Nothing -> pure ()
        pure $ mergePRRender output

data Tools mode = Tools
  { forkWave    :: mode :- RootForkWave,
    spawnGemini :: mode :- RootSpawnGemini,
    mergePr     :: mode :- RootMergePR,
    sendMessage :: mode :- SendMessage
  }
  deriving (Generic)

rootStopCheck :: Eff Effects StopHookOutput
rootStopCheck = do
  branch <- getCurrentBranch
  if branch `elem` ["main", "master"]
    then pure allowStopResponse
    else do
      result <- checkExit @TLPhase @TLEvent TLPlanning
      case result of
        MustBlock msg -> pure $ blockStopResponse msg
        ShouldNudge msg -> pure $ StopHookOutput Allow (Just msg)
        Clean -> do
          nudge <- checkUncommittedWork branch
          case nudge of
            Just msg -> pure $ StopHookOutput Allow (Just msg)
            Nothing -> pure allowStopResponse

config :: RoleConfig (Tools AsHandler)
config =
  RoleConfig
    { roleName = "root",
      tools = Tools
        { forkWave    = mkHandler @RootForkWave,
          spawnGemini = mkHandler @RootSpawnGemini,
          mergePr     = mkHandler @RootMergePR,
          sendMessage = mkHandler @SendMessage
        },
      hooks = HookConfig
        { preToolUse       = \_ -> pure (allowResponse Nothing),
          postToolUse      = teamRegistrationPostToolUse,
          onStop           = \_ -> rootStopCheck,
          onSubagentStop   = \_ -> rootStopCheck,
          onSessionStart   = defaultSessionStartHook
        },
      eventHandlers = prReviewEventHandlers
    }
