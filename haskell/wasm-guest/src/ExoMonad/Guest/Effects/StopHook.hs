{-# LANGUAGE OverloadedStrings #-}

module ExoMonad.Guest.Effects.StopHook
  ( runStopHookChecks,
    checkUncommittedWork,
    getCurrentBranch,

    -- * Agent identity helpers
    getAgentId,
  )
where

import Control.Monad (void)
import Control.Monad.Freer (Eff)
import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Effects.Git qualified as Git
import Effects.Log qualified as Log
import ExoMonad.Effects.Git (GitGetBranch, GitGetStatus, GitHasUnpushedCommits)
import ExoMonad.Effects.Log (LogEmitEvent)
import ExoMonad.Guest.Lifecycle.DevState (SomeDevState, describeDevPhase)
import ExoMonad.Guest.Lifecycle.DevTransitions qualified as DevTransitions
import ExoMonad.Guest.Lifecycle.PhaseEffect (StopCheckResult (..), describeStopResult, getDevPhase)
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect, suspendEffect_)
import ExoMonad.Guest.Types (StopDecision (..), StopHookOutput (..), allowStopResponse, blockStopResponse)
import ExoMonad.Types (Effects)
import System.Environment (lookupEnv)

-- ============================================================================
-- Main Check Logic
-- ============================================================================

runStopHookChecks :: Eff Effects StopHookOutput
runStopHookChecks = do
  branch <- getCurrentBranch
  if branch `elem` ["main", "master"]
    then pure allowStopResponse
    else do
      mPhase <- getDevPhase
      result <- case mPhase of
        Just phase -> case DevTransitions.canExit phase of
          r@(MustBlock _) -> pure r
          ShouldNudge msg -> pure (ShouldNudge msg)
          Clean -> do
            nudge <- checkUncommittedWork branch
            case nudge of
              Just msg -> pure (ShouldNudge msg)
              Nothing -> pure Clean
        Nothing -> do
          -- No phase set yet — fall back to git status check
          nudge <- checkUncommittedWork branch
          case nudge of
            Just msg -> pure (ShouldNudge msg)
            Nothing -> pure Clean

      -- Log to event log for observability
      let eventPayload = BSL.toStrict $ Aeson.encode $
            object ["branch" .= branch, "result" .= describeStopResult result, "phase" .= (Aeson.toJSON <$> (mPhase :: Maybe SomeDevState))]
      void $ suspendEffect_ @LogEmitEvent (Log.EmitEventRequest
        { Log.emitEventRequestEventType = "agent.stop_check",
          Log.emitEventRequestPayload = eventPayload,
          Log.emitEventRequestTimestamp = 0
        })

      case result of
        MustBlock msg -> pure $ blockStopResponse msg
        ShouldNudge msg -> pure $ StopHookOutput Allow (Just msg)
        Clean -> pure allowStopResponse

-- | Check for uncommitted/unpushed work and nudge if found.
checkUncommittedWork :: Text -> Eff Effects (Maybe Text)
checkUncommittedWork branch = do
  statusResult <- suspendEffect @GitGetStatus (Git.GetStatusRequest {Git.getStatusRequestWorkingDir = "."})
  let hasUncommitted = case statusResult of
        Right resp -> not (null (Git.getStatusResponseDirtyFiles resp))
                      || not (null (Git.getStatusResponseStagedFiles resp))
        _ -> False

  unpushedResult <- suspendEffect @GitHasUnpushedCommits (Git.HasUnpushedCommitsRequest {Git.hasUnpushedCommitsRequestWorkingDir = ".", Git.hasUnpushedCommitsRequestRemote = "origin"})
  let hasUnpushed = case unpushedResult of
        Right resp -> Git.hasUnpushedCommitsResponseHasUnpushed resp
        _ -> False

  if hasUncommitted
    then pure $ Just $ "You have uncommitted changes on " <> branch <> " but no PR filed. Commit and file a PR before stopping."
    else if hasUnpushed
      then pure $ Just $ "Commits on " <> branch <> " aren't in a PR yet. File a PR before stopping."
      else pure Nothing

-- ============================================================================
-- Agent Identity Helpers
-- ============================================================================

-- | Read the agent's identity from EXOMONAD_AGENT_ID env var.
getAgentId :: IO (Maybe Text)
getAgentId = fmap (fmap T.pack) (lookupEnv "EXOMONAD_AGENT_ID")

-- | Get the current git branch name, defaulting to "unknown" on error.
getCurrentBranch :: Eff Effects Text
getCurrentBranch = do
  result <- suspendEffect @GitGetBranch (Git.GetBranchRequest {Git.getBranchRequestWorkingDir = "."})
  case result of
    Right resp -> pure $ TL.toStrict (Git.getBranchResponseBranch resp)
    Left _ -> pure "unknown"
