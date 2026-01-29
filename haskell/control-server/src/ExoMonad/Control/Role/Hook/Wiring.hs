{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Control.Role.Hook.Wiring
  ( commonHooks
  ) where

import Control.Monad.Freer (Eff, Member, LastMember)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

import ExoMonad.Graph.Generic (AsHandler, HookHandler(..))
import ExoMonad.Control.Role.Hook.Definitions (CommonHooks(..))
import ExoMonad.Control.Role.Types
import ExoMonad.Control.Hook.SessionStart (sessionStartLogic)
import ExoMonad.Control.Hook.Policy (HookDecision(..), evaluatePolicy, HookPolicy(..))
import ExoMonad.Control.StopHook.Types (StopHookContext(..), TemplateName, AgentState(..))
import ExoMonad.Control.StopHook.Handlers (stopHookHandlers)
import ExoMonad.Control.StopHook.Templates (renderStopHookTemplate)
import ExoMonad.Control.Workflow.Store (WorkflowStore, getWorkflowState, updateWorkflowState)

import ExoMonad.Effects.Env (Env, lookupEnv)
import ExoMonad.Effects.Git (Git, getWorktreeInfo, WorktreeInfo(..))
import ExoMonad.Effects.GitHub (GitHub) -- Need GitHub for sessionStartLogic
import ExoMonad.Effect.Types (Log, logDebug, logInfo, logError) 
import ExoMonad.Effects.Zellij (Zellij)
import ExoMonad.Control.Types (ServerConfig(..))
import ExoMonad.Control.ExoTools (parseIssueNumber)
import ExoMonad.Graph.Interpret (runGraph)
import ExoMonad.Effect.NodeMeta (NodeMeta, runNodeMeta, defaultNodeMeta)
import Control.Monad.Freer.State (State, runState)
import Control.Monad.Freer.Reader (Reader, ask)
import OpenTelemetry.Trace (Tracer)
import ExoMonad.Control.RoleConfig (roleFromText)

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
  , LastMember IO es
  ) => CommonHooks (AsHandler es)
commonHooks = CommonHooks
  { sessionStart = HookHandler handleSessionStart
  , preToolUse   = HookHandler handlePreToolUse
  , postToolUse  = HookHandler handlePostToolUse
  , stop         = HookHandler handleStop
  , sessionEnd   = HookHandler handleSessionEnd
  , notification = HookHandler handleNotification
  , subagentStop = HookHandler handleSessionEnd 
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
  mContext <- sessionStartLogic tracer currentRole (T.pack input.ssiCwd)
  
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
  ) => StopInput -> Eff es StopResponse
handleStop input = do
  -- Stop logic placeholder
  pure $ StopResponse True Nothing

handleSessionEnd :: SessionEndInput -> Eff es ()
handleSessionEnd _ = pure ()

handleNotification :: Notification -> Eff es ()
handleNotification _ = pure ()


