{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | TL role config: spawn, PR, merge tools with state transitions and stop hook checks.
module TLRole (config, Tools) where

import Control.Monad (void, forM_, when)
import Control.Monad.Freer (Eff)
import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import ExoMonad
import ExoMonad.Guest.StateMachine (applyEvent, StopCheckResult(..), checkExit)
import ExoMonad.Guest.Effects.StopHook (checkUncommittedWork, getCurrentBranch)
import ExoMonad.Guest.Tools.FilePR (filePRCore, filePRDescription, filePRSchema, FilePRArgs, FilePROutput (..))
import ExoMonad.Guest.Tools.Events
  ( notifyParentCore, notifyParentDescription, notifyParentSchema, NotifyParentArgs (..),
    shutdownCore, shutdownDescription, shutdownSchema, ShutdownArgs
  )
import ExoMonad.Guest.Tools.MergePR (mergePRCore, mergePRDescription, mergePRSchema, mergePRRender, MergePRArgs (..), MergePROutput (..), extractSlug)
import ExoMonad.Guest.Tools.Spawn
  ( forkWaveCore, forkWaveDescription, forkWaveSchema, forkWaveRender, ForkWaveArgs (..), ForkWaveResult (..),
    spawnGeminiCore, spawnGeminiDescription, spawnGeminiSchema, SpawnGemini, SpawnGeminiArgs,
    spawnLeafRender,
    spawnAcpCore, SpawnAcpArgs
  )
import ExoMonad.Guest.Effects.AgentControl (SpawnResult (..))
import ExoMonad.Guest.Types (StopDecision(..), StopHookOutput(..), blockStopResponse, allowStopResponse, allowResponse)
import ExoMonad.Types (HookConfig (..), Effects, defaultSessionStartHook, teamRegistrationPostToolUse)
import PRReviewHandler (prReviewEventHandlers)
import TLPhase (TLPhase (..), TLEvent (..), ChildHandle (..))

-- | TL-specific file_pr: files PR, transitions TLPhase.
data TLFilePR

instance MCPTool TLFilePR where
  type ToolArgs TLFilePR = FilePRArgs
  toolName = "file_pr"
  toolDescription = filePRDescription
  toolSchema = filePRSchema
  toolHandlerEff args = do
    result <- filePRCore args
    case result of
      Left err -> pure $ errorResult err
      Right output -> do
        void $ applyEvent @TLPhase @TLEvent TLPlanning
          (OwnPRFiled (fpoNumber output) (fpoUrl output) (fpoHeadBranch output))
        pure $ successResult (Aeson.toJSON output)

-- | TL-specific merge_pr: merges child PR, transitions TLPhase via ChildCompleted.
data TLMergePR

instance MCPTool TLMergePR where
  type ToolArgs TLMergePR = MergePRArgs
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

-- | TL-specific fork_wave: spawns Claude subtrees, fires ChildSpawned per child.
data TLForkWave

instance MCPTool TLForkWave where
  type ToolArgs TLForkWave = ForkWaveArgs
  toolName = "fork_wave"
  toolDescription = forkWaveDescription
  toolSchema = forkWaveSchema
  toolHandlerEff args = do
    result <- forkWaveCore args
    case result of
      Left err -> pure $ errorResult err
      Right fwResult -> do
        forM_ (fwrSpawned fwResult) $ \(slug, sr) -> do
          let handle = ChildHandle
                { chSlug = slug
                , chBranch = branchName sr
                , chAgentType = agentTypeResult sr
                }
          void $ applyEvent @TLPhase @TLEvent TLPlanning (ChildSpawned handle)
        pure $ forkWaveRender fwResult

-- | TL-specific spawn_gemini: worktree/standalone fire ChildSpawned; inline is ephemeral.
data TLSpawnGemini

instance MCPTool TLSpawnGemini where
  type ToolArgs TLSpawnGemini = SpawnGeminiArgs
  toolName = "spawn_gemini"
  toolDescription = spawnGeminiDescription
  toolSchema = spawnGeminiSchema
  toolHandlerEff args = do
    result <- spawnGeminiCore args
    case result of
      Left err -> pure $ errorResult err
      Right (Just (slug, sr)) -> do
        let handle = ChildHandle
              { chSlug = slug
              , chBranch = branchName sr
              , chAgentType = agentTypeResult sr
              }
        void $ applyEvent @TLPhase @TLEvent TLPlanning (ChildSpawned handle)
        pure $ spawnLeafRender (Right (slug, sr))
      Right Nothing ->
        pure $ successResult $ object ["spawned" .= True]

-- | TL notify_parent: thin wrapper, no phase transitions.
data TLNotifyParent

instance MCPTool TLNotifyParent where
  type ToolArgs TLNotifyParent = NotifyParentArgs
  toolName = "notify_parent"
  toolDescription = notifyParentDescription
  toolSchema = notifyParentSchema
  toolHandlerEff args = do
    result <- notifyParentCore args
    case result of
      Left err -> pure $ errorResult err
      Right _ -> pure $ successResult $ object ["success" .= True]

data Tools mode = Tools
  { forkWave :: mode :- TLForkWave,
    spawnGemini :: mode :- TLSpawnGemini,
    pr :: mode :- TLFilePR,
    mergePr :: mode :- TLMergePR,
    notifyParent :: mode :- TLNotifyParent,
    sendMessage :: mode :- SendMessage
  }
  deriving (Generic)

tlStopCheck :: Eff Effects StopHookOutput
tlStopCheck = do
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
    { roleName = "tl",
      tools =
        Tools
          { forkWave = mkHandler @TLForkWave,
            spawnGemini = mkHandler @TLSpawnGemini,
            pr = mkHandler @TLFilePR,
            mergePr = mkHandler @TLMergePR,
            notifyParent = mkHandler @TLNotifyParent,
            sendMessage = mkHandler @SendMessage
          },
      hooks =
        HookConfig
          { preToolUse = \_ -> pure (allowResponse Nothing),
            postToolUse = teamRegistrationPostToolUse,
            onStop = \_ -> tlStopCheck,
            onSubagentStop = \_ -> tlStopCheck,
            onSessionStart = defaultSessionStartHook
          },
      eventHandlers = prReviewEventHandlers
    }
