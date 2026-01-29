{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Hook event handler.
--
-- Handles hook events from Claude Code via exomonad.
-- Dispatches to Role-specific hooks using Generic dispatch.
module ExoMonad.Control.Handler.Hook
  ( handleHook
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (fromMaybe)
import System.IO (hFlush, stdout)
import Control.Exception (SomeException, try, throwIO)
import Control.Concurrent.STM (TVar)
import Control.Monad (forM_)
import Control.Monad.Freer (runM)
import Control.Monad.Freer.Reader (runReader)
import Data.Aeson (Value(..), toJSON, fromJSON, Result(..))
import OpenTelemetry.Trace hiding (Error)
import qualified OpenTelemetry.Context.ThreadLocal as Context

import ExoMonad.Control.Logging (Logger)
import ExoMonad.Control.Protocol hiding (role)
import ExoMonad.Control.Types (ServerConfig(..))
import ExoMonad.Control.Hook.CircuitBreaker (CircuitBreakerMap)
import ExoMonad.Control.RoleConfig (Role(..))
import ExoMonad.Control.Role.Schema (camelToSnake)

-- Dispatch Imports
import ExoMonad.Control.Runtime (AppEffects)
import ExoMonad.Control.Role.Hook.Dispatch (dispatchHook, HookDispatchResult(..))
import ExoMonad.Control.Role.Handlers (tlHandlers, devHandlers, pmHandlers)
import ExoMonad.Control.Role.Definition.TL (TLRole(..))
import ExoMonad.Control.Role.Definition.Dev (DevRole(..))
import ExoMonad.Control.Role.Definition.PM (PMRole(..))
import ExoMonad.Control.Role.Types
  ( SessionStartResponse(..)
  , PreToolUseResponse(..)
  , StopResponse(..)
  )

-- Interpreter Imports (for stack construction)
import qualified ExoMonad.Control.Runtime.Paths as Paths
import ExoMonad.Control.Hook.GitHubRetry (withRetry, defaultRetryConfig, RetryConfig(..))
import ExoMonad.Control.Effects.SshExec (runSshExec)
import ExoMonad.Control.Effects.Effector (runEffectorIO)
import ExoMonad.Control.Effects.Cabal (runCabalRemote)
import ExoMonad.Control.Effects.Justfile (runJustfileRemote)
import ExoMonad.Git.Interpreter (runGitIO)
import ExoMonad.GitHub.Interpreter (runGitHubIO, defaultGitHubConfig)
import ExoMonad.Worktree.Interpreter (runWorktreeIO, defaultWorktreeConfig)
import ExoMonad.FileSystem.Interpreter (runFileSystemIO)
import ExoMonad.Env.Interpreter (runEnvIO)
import ExoMonad.Zellij.Interpreter (runZellijIO)
import ExoMonad.Control.Effects.DockerCtl (runDockerCtl)
import ExoMonad.Gemini.Interpreter (runGeminiIO)
import ExoMonad.Control.TUIInterpreter (runTUIFifo)
import ExoMonad.Control.Effects.Git (runGitRemote)
import ExoMonad.Effect.Types (runLog, LogLevel(Debug), runTime)

-- | Handle a hook event.
handleHook :: Logger -> Tracer -> ServerConfig -> HookInput -> Runtime -> Role -> Maybe Text -> CircuitBreakerMap -> TVar [AgentStatus] -> IO ControlResponse
handleHook logger tracer config input _ agentRole mContainerId cbMap agentStore = do
  TIO.putStrLn $ "  session=" <> input.sessionId
  TIO.putStrLn $ "  cwd=" <> input.cwd
  TIO.putStrLn $ "  role=" <> T.pack (show agentRole)
  hFlush stdout

  let spanName = "hook." <> input.hookEventName
  
  -- Manual span management
  ctx <- Context.getContext
  span_ <- createSpan tracer ctx spanName defaultSpanArguments
  
  result <- try $ do
    addAttribute span_ "session.id" input.sessionId
    addAttribute span_ "jsonl.file" input.transcriptPath
    forM_ input.toolUseId $ \tid -> addAttribute span_ "tool_use_id" tid
    forM_ input.toolName $ \tn -> addAttribute span_ "tool_name" tn
    
    let eventName = camelToSnake input.hookEventName
        inputJson = toJSON input

    -- Dispatch to appropriate role handler
    let dispatchResult = case agentRole of
          TL  -> let TLRole{tlHooks=h}  = tlHandlers @AppEffects in dispatchHook h eventName inputJson
          Dev -> let DevRole{devHooks=h} = devHandlers @AppEffects in dispatchHook h eventName inputJson
          PM  -> let PMRole{pmHooks=h}  = pmHandlers @AppEffects in dispatchHook h eventName inputJson
    
    case dispatchResult of
      HookNotFound -> do
        -- Fallback for events not in hook record (e.g. legacy/internal events)
        pure $ hookSuccess $ makeDefaultResponse input.hookEventName input

      HookParseError msg -> do
        let errMsg = "Hook parse error for " <> eventName <> ": " <> msg
        TIO.putStrLn $ "  [HOOK] " <> errMsg
        hFlush stdout
        -- Fail closed on parse error
        throwIO (userError (T.unpack errMsg))

      HookFound action -> do
        -- Run the action with the application effect stack
        -- We construct the stack manually to allow dynamic Git interpreter (Remote vs Local)
        
        -- Resolve paths
        binDir <- Paths.dockerBinDir
        let dockerCtlPath = Paths.dockerCtlBin binDir
        let repoRoot = "." 
        let ghConfig = fromMaybe defaultGitHubConfig config.githubConfig
        let retryCfg = defaultRetryConfig { tracer = Just tracer }

        runM
          $ runLog Debug
          $ runTime
          $ runReader cbMap
          $ runReader tracer
          $ runReader config
          $ runTUIFifo logger
          $ runEnvIO
          $ runFileSystemIO
          $ runGitHubIO ghConfig
          $ withRetry retryCfg
          $ runZellijIO
          $ runSshExec logger dockerCtlPath
          -- Dynamic Git interpreter based on container ID
          $ case mContainerId of 
               Just c -> runGitRemote c "."
               Nothing -> runGitIO
          $ runWorktreeIO (defaultWorktreeConfig repoRoot)
          $ runEffectorIO logger
          $ runCabalRemote mContainerId
          $ runJustfileRemote (fromMaybe "" mContainerId) ""
          $ runDockerCtl logger dockerCtlPath agentStore
          $ runGeminiIO
          $ do
              val <- action
              pure $ hookSuccess $ makeResponseFromValue input.hookEventName val

  endSpan span_ Nothing
  
  case result of
    Left (e :: SomeException) -> do
      recordException span_ mempty Nothing e
      throwIO e
    Right r -> pure r

-- | Make response from generic Value result
makeResponseFromValue :: Text -> Value -> HookOutput
makeResponseFromValue eventName val = case eventName of
  "SessionStart" -> case fromJSON val of
      Success (SessionStartResponse cont ctx) -> defaultOutput
        { continue_ = cont
        , hookSpecificOutput = Just $ SessionStartOutput ctx
        }
      Error _ -> defaultOutput
  
  "PreToolUse" -> case fromJSON val of
      Success resp -> case resp of
         PTUAllow -> defaultOutput
         PTUDeny r -> denyPreToolUse r
         PTUTransform v -> allowPreToolUse Nothing (Just v)
      Error _ -> defaultOutput

  "Stop" -> case fromJSON val of
      Success (StopResponse allow msg) -> defaultOutput
        { continue_ = allow
        , stopReason = msg -- Or systemMessage?
        }
      Error _ -> defaultOutput

  "SessionEnd" -> defaultOutput { hookSpecificOutput = Just SessionEndOutput }
  "SubagentStop" -> defaultOutput { hookSpecificOutput = Just $ SubagentStopOutput Nothing Nothing }
  "Notification" -> defaultOutput { hookSpecificOutput = Just NotificationOutput }
  
  _ -> defaultOutput

-- | Legacy response maker for fallbacks
makeDefaultResponse :: Text -> HookInput -> HookOutput
makeDefaultResponse eventName _ = case eventName of
  "PostToolUse" -> allowPostToolUse Nothing
  "PermissionRequest" -> defaultOutput
    {
      hookSpecificOutput = Just $ PermissionRequestOutput $ Allow Nothing
    }
  "UserPromptSubmit" -> defaultOutput
    {
      hookSpecificOutput = Just $ UserPromptSubmitOutput Nothing
    }
  "PreCompact" -> defaultOutput
    {
      hookSpecificOutput = Just PreCompactOutput
    }
  _ -> defaultOutput

-- | Default output (continue, no specific output)
defaultOutput :: HookOutput
defaultOutput = HookOutput
  {
    continue_ = True
  , stopReason = Nothing
  , suppressOutput = Nothing
  , systemMessage = Nothing
  , hookSpecificOutput = Nothing
  , decision = Nothing
  , reason = Nothing
  }