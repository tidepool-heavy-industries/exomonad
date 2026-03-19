{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Root TL role: orchestration-only. No file_pr, notify_parent, or shutdown.
--   Used for the root human-facing TL window (exomonad init).
module RootRole (config, Tools) where

import Control.Monad (void, forM_, when)
import ExoMonad
import ExoMonad.Guest.StateMachine (applyEvent)
import ExoMonad.Guest.Effects.StopHook (getCurrentBranch)
import ExoMonad.Guest.Tools.MergePR (mergePRCore, mergePRDescription, mergePRSchema, mergePRRender, MergePRArgs (..), MergePROutput (..), extractSlug)
import ExoMonad.Guest.Tools.Spawn
  ( forkWaveCore, forkWaveDescription, forkWaveSchema, forkWaveRender, ForkWaveArgs (..), ForkWaveResult (..),
    spawnGeminiCore, spawnGeminiDescription, spawnGeminiSchema, spawnLeafRender, SpawnGeminiArgs (..),
    spawnWorkerToolCore, spawnWorkerToolDescription, spawnWorkerToolSchema, SpawnWorkerToolArgs
  )
import ExoMonad.Guest.Effects.AgentControl (SpawnResult (..))
import ExoMonad.Guest.Types (allowResponse)
import ExoMonad.Types (HookConfig (..), defaultSessionStartHook, teamRegistrationPostToolUse)
import PRReviewHandler (prReviewEventHandlers)
import TLPhase (TLPhase (..), TLEvent (..), ChildHandle (..))
import TLStopCheck (tlStopCheck)

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
          branch <- getCurrentBranch
          void $ applyEvent @TLPhase @TLEvent branch TLPlanning (ChildSpawned handle)
        pure $ forkWaveRender fwResult

data RootSpawnGemini
instance MCPTool RootSpawnGemini where
  type ToolArgs RootSpawnGemini = SpawnGeminiArgs
  toolName = "spawn_gemini"
  toolDescription = spawnGeminiDescription
  toolSchema = spawnGeminiSchema
  toolHandlerEff args = do
    result <- spawnGeminiCore args
    case result of
      Left err -> pure $ errorResult err
      Right (slug, sr) -> do
        let handle = ChildHandle { chSlug = slug, chBranch = branchName sr, chAgentType = agentTypeResult sr }
        branch <- getCurrentBranch
        void $ applyEvent @TLPhase @TLEvent branch TLPlanning (ChildSpawned handle)
        pure $ spawnLeafRender (Right (slug, sr))

data RootSpawnWorker
instance MCPTool RootSpawnWorker where
  type ToolArgs RootSpawnWorker = SpawnWorkerToolArgs
  toolName = "spawn_worker"
  toolDescription = spawnWorkerToolDescription
  toolSchema = spawnWorkerToolSchema
  toolHandlerEff args = spawnWorkerToolCore args

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
            Just slug -> do
              branch <- getCurrentBranch
              void $ applyEvent @TLPhase @TLEvent branch TLPlanning (ChildCompleted slug)
            Nothing -> pure ()
        pure $ mergePRRender output

data Tools mode = Tools
  { forkWave    :: mode :- RootForkWave,
    spawnGemini :: mode :- RootSpawnGemini,
    spawnWorker :: mode :- RootSpawnWorker,
    mergePr     :: mode :- RootMergePR,
    sendMessage :: mode :- SendMessage
  }
  deriving (Generic)

config :: RoleConfig (Tools AsHandler)
config =
  RoleConfig
    { roleName = "root",
      tools = Tools
        { forkWave    = mkHandler @RootForkWave,
          spawnGemini = mkHandler @RootSpawnGemini,
          spawnWorker = mkHandler @RootSpawnWorker,
          mergePr     = mkHandler @RootMergePR,
          sendMessage = mkHandler @SendMessage
        },
      hooks = HookConfig
        { preToolUse       = \_ -> pure (allowResponse Nothing),
          postToolUse      = teamRegistrationPostToolUse,
          onStop           = \_ -> tlStopCheck,
          onSubagentStop   = \_ -> tlStopCheck,
          onSessionStart   = defaultSessionStartHook
        },
      eventHandlers = prReviewEventHandlers
    }
