{-# LANGUAGE OverloadedStrings #-}

-- | HTTP-native hook configuration for dev agents.
--
-- In the HTTP model, hooks run server-side instead of embedded in the
-- client's local WASM. The permission cascade is a passthrough initially
-- (always returns Allowed) and will be built incrementally.
module HttpDevHooks
  ( httpDevHooks,
  )
where

import Control.Monad (void)
import Control.Monad.Freer (Eff)
import Data.Aeson qualified as Aeson
import Data.Maybe (fromMaybe)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Effects.Log qualified as Log
import ExoMonad.Effects.Log (LogInfo)
import ExoMonad.Guest.Effects.StopHook (runStopHookChecks)
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect_)
import ExoMonad.Guest.Types (HookInput (..), HookOutput, allowResponse, denyResponse, postToolUseResponse)
import ExoMonad.Permissions (PermissionCheck (..), checkAgentPermissions)
import ExoMonad.Types (HookConfig (..), HookEffects, defaultSessionStartHook)

-- | Hook config for HTTP-native dev agents.
--
-- Uses the permission cascade for preToolUse (passthrough initially)
-- and existing stop hook checks for onStop/onSubagentStop.
httpDevHooks :: HookConfig
httpDevHooks =
  HookConfig
    { preToolUse = permissionCascade,
      postToolUse = \_ -> pure (postToolUseResponse Nothing),
      onStop = \_ -> runStopHookChecks,
      onSubagentStop = \_ -> runStopHookChecks,
      onSessionStart = defaultSessionStartHook
    }

-- | Three-tier permission cascade for preToolUse hooks.
--
-- Currently a passthrough (auto-allow via checkAgentPermissions).
-- The cascade will eventually be:
--
-- 1. Agent's own permission set (auto-approve)
-- 2. Escalate to TL (auto-approve within TL scope)
-- 3. Escalate to human (popup in Zellij)
permissionCascade :: HookInput -> Eff HookEffects HookOutput
permissionCascade hookInput = do
  let tool = fromMaybe "" (hiToolName hookInput)
      args = fromMaybe (Aeson.Object mempty) (hiToolInput hookInput)
      argsJson = TLE.decodeUtf8 $ Aeson.encode args
  void $ suspendEffect_ @LogInfo $ Log.InfoRequest
    { Log.infoRequestMessage = "[BeforeTool] tool=" <> TL.fromStrict tool <> " input=" <> argsJson
    , Log.infoRequestFields = ""
    }
  case checkAgentPermissions "dev" tool args of
    Allowed -> pure (allowResponse Nothing)
    Escalate -> pure (allowResponse (Just "escalation-needed"))
    Denied reason -> pure (denyResponse reason)
