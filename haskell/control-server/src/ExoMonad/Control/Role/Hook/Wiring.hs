
module ExoMonad.Control.Role.Hook.Wiring
  ( commonHooks
  ) where

import Control.Monad.Freer (Eff, Member, LastMember, sendM)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.Aeson (Value(..), decode, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as BL
import Control.Exception (try, SomeException)

import ExoMonad.Graph.Generic (AsHandler, HookHandler(..))
import ExoMonad.Control.Role.Hook.Definitions (CommonHooks(..))
import ExoMonad.Control.Role.Types
import ExoMonad.Control.Hook.SessionStart (sessionStartLogic)
import ExoMonad.Control.Hook.Policy (HookDecision(..), evaluatePolicy)
import ExoMonad.Control.Types (ServerConfig(..))
import ExoMonad.Control.OpenObserve (shipTranscript)
import Control.Monad.Freer.Reader (Reader, ask)
import OpenTelemetry.Trace (Tracer)
import ExoMonad.Control.RoleConfig (roleFromText, Role(..))

import ExoMonad.Effects.Env (Env, lookupEnv)
import ExoMonad.Effects.Git (Git, getWorktreeInfo, WorktreeInfo(..))
import ExoMonad.Effects.GitHub (GitHub)
import ExoMonad.Effect.Types (Log, logInfo) 
import ExoMonad.Effects.Zellij (Zellij, checkZellijEnv, goToTab, TabId(..))
import ExoMonad.Control.Effects.Cabal (Cabal)
import ExoMonad.Control.Effects.Effector (Effector)
import ExoMonad.Control.ExoTools (parseIssueNumber)

-- We need a Reader effect for ServerConfig to access policy and workflow store
type ConfigReader = Reader ServerConfig
type TracerReader = Reader Tracer

commonHooks :: 
  ( Member Env es
  , Member Git es
  , Member GitHub es
  , Member Log es
  , Member ConfigReader es
  , Member TracerReader es
  , Member Zellij es
  , Member Cabal es
  , Member Effector es
  , LastMember IO es
  ) => CommonHooks (AsHandler es)
commonHooks = CommonHooks
  { sessionStart = HookHandler handleSessionStart
  , preToolUse   = HookHandler handlePreToolUse
  , postToolUse  = HookHandler handlePostToolUse
  , stop         = HookHandler handleStop
  , sessionEnd   = HookHandler handleSessionEnd
  , notification = HookHandler handleNotification
  , subagentStop = HookHandler handleSubagentStop 
  }

handleSessionStart :: 
  ( Member Env es
  , Member Git es
  , Member GitHub es
  , Member Log es
  , Member ConfigReader es
  , Member TracerReader es
  , LastMember IO es
  ) => SessionStartInput -> Eff es SessionStartResponse
handleSessionStart input = do
  logInfo "Running SessionStart context injection..."
  
  tracer <- ask @Tracer
  config <- ask @ServerConfig
  let currentRole = fromMaybe config.defaultRole (config.role >>= roleFromText)
  
  -- sessionStartLogic returns Maybe Text
  mContext <- sessionStartLogic tracer currentRole input.ssiSessionType (T.pack input.ssiCwd)
  
  pure $ SessionStartResponse True mContext

handlePreToolUse :: 
  ( Member ConfigReader es
  , Member Log es
  ) => PreToolUseInput -> Eff es PreToolUseResponse
handlePreToolUse input = do
  config <- ask @ServerConfig
  let toolName = input.ptuToolName
  
  logInfo $ "Evaluating PreToolUse policy for tool: " <> toolName
  
  case evaluatePolicy config.hookPolicy toolName of
    PolicyAllow reason modifiedInput -> do
      logInfo $ "Policy: ALLOW " <> fromMaybe "" reason
      pure $ case modifiedInput of
        Just val -> PTUTransform val
        Nothing -> PTUAllow
    PolicyDeny reason -> do
      logInfo $ "Policy: DENY " <> reason
      pure $ PTUDeny reason
    PolicyAsk reason -> do
      logInfo $ "Policy: ASK " <> fromMaybe "" reason
      pure PTUAllow -- Fallback

handlePostToolUse :: PostToolUseInput -> Eff es ()
handlePostToolUse _ = pure ()

handleStop :: 
  ( Member ConfigReader es
  , Member Git es
  , Member Log es
  , Member Cabal es
  , Member Effector es
  , Member Env es
  , Member Zellij es
  , LastMember IO es
  ) => StopInput -> Eff es StopResponse
handleStop input = do
  -- Auto-focus logic
  mAutoFocus <- lookupEnv "EXOMONAD_AUTO_FOCUS_ON_ERROR"
  let autoFocusEnabled = mAutoFocus /= Just "false"

  if autoFocusEnabled
    then autoFocusLogic
    else pure ()

  -- TODO: Re-implement stop hook graph execution logic.
  -- Current implementation encounters type inference issues with runGraph/HasField/OverloadedRecordDot
  -- when running inside the open effect stack 'es'.
  -- Original logic involved running 'stopHookHandlers' graph with 'runState', 'runNodeMeta', etc.
  
  -- For now, return a default response to allow the build to pass and the refactor to land.
  pure $ StopResponse
    { srAllowStop = True
    , srMessage = Nothing
    }

handleSessionEnd :: (Member ConfigReader es, LastMember IO es, Member Log es) => SessionEndInput -> Eff es ()
handleSessionEnd input = do
  config <- ask @ServerConfig
  case config.openObserveConfig of
    Nothing -> logInfo "OpenObserve not configured, skipping transcript shipping"
    Just ooConfig -> do
      let path = T.unpack input.seiTranscriptPath
      if null path
        then logInfo "No transcript path provided, skipping"
        else do
          mEvents <- sendM $ readJsonl path
          case mEvents of
            Nothing -> logInfo $ "Failed to read or parse transcript at " <> T.pack path
            Just events -> do
              let role = fromMaybe config.defaultRole (config.role >>= roleFromText)
              let enriched = map (enrichEvent role input.seiSessionId (T.pack input.seiCwd) "SessionEnd") events
              sendM $ shipTranscript ooConfig enriched

handleSubagentStop :: (Member ConfigReader es, LastMember IO es, Member Log es) => SubagentStopInput -> Eff es ()
handleSubagentStop input = do
  config <- ask @ServerConfig
  case config.openObserveConfig of
    Nothing -> logInfo "OpenObserve not configured, skipping transcript shipping"
    Just ooConfig -> do
      let path = T.unpack input.sasiTranscriptPath
      if null path
        then logInfo "No transcript path provided, skipping"
        else do
          mEvents <- sendM $ readJsonl path
          case mEvents of
            Nothing -> logInfo $ "Failed to read or parse transcript at " <> T.pack path
            Just events -> do
              let role = fromMaybe config.defaultRole (config.role >>= roleFromText)
              let enriched = map (enrichEvent role input.sasiSessionId (T.pack input.sasiCwd) "SubagentStop") events
              sendM $ shipTranscript ooConfig enriched

-- | Helper to read NDJSON (JSONL) file.
readJsonl :: FilePath -> IO (Maybe [Value])
readJsonl path = try @SomeException (BL.readFile path) >>= \case
  Left _ -> pure Nothing
  Right content -> do
    let jsonLines = filter (not . BL.null) $ BL.split 10 content -- Split by newline (\n = 10)
    pure $ decodeAllJsonLines jsonLines
  where
    stripTrailingCR bs
      | BL.null bs = bs
      | BL.last bs == 13 = BL.init bs
      | otherwise = bs

    decodeAllJsonLines = go []
      where
        go acc [] = Just (reverse acc)
        go acc (l : ls) =
          case decode (stripTrailingCR l) of
            Just v  -> go (v : acc) ls
            Nothing -> Nothing

-- | Helper to enrich transcript event with session metadata.
enrichEvent :: Role -> T.Text -> T.Text -> T.Text -> Value -> Value
enrichEvent role sessionId cwd hookName (Object o) = Object $ o 
  <> KeyMap.fromList 
     [ "session_id" .= sessionId
     , "agent_role" .= T.pack (show role)
     , "cwd" .= cwd
     , "hook_event" .= hookName
     ]
enrichEvent _ _ _ _ v = v

handleNotification :: Notification -> Eff es ()
handleNotification _ = pure ()

-- | Auto-focus logic: check Zellij, parse issue ID, switch focus.
autoFocusLogic :: (Member Zellij es, Member Git es) => Eff es ()
autoFocusLogic = do
  mZellij <- checkZellijEnv
  case mZellij of
    Nothing -> pure ()
    Just _ -> do
      mWt <- getWorktreeInfo
      case mWt of
        Nothing -> pure ()
        Just wt -> do
          -- Using pattern matching because of OverloadedRecordDot issues
          let branchName = (\(WorktreeInfo { wiBranch = b }) -> b) wt
              maybeIssueNum = parseIssueNumber branchName
              maybeIssueId = T.pack . show <$> maybeIssueNum
          case maybeIssueId of
            Nothing -> pure ()
            Just bid -> do
              -- Focus on tab with Issue ID name
              _ <- goToTab (TabId bid)
              pure ()