{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | Role registry for the e2e-test WASM.
-- Only two roles: root (with PII rewriting hooks) and testrunner.
module AllRoles
  ( allConfigs,
    lookupRole,
    SomeRoleConfig (..),

    -- * Accessors
    roleDispatch,
    roleListTools,
    roleHooks,
    roleToolsMCP,
    roleEventHandlers,
  )
where

import Data.Aeson (Value)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import ExoMonad.Guest.Tool.Class (MCPCallOutput, ToolDefinition, WasmResult, toMCPFormat)
import ExoMonad.Guest.Tool.Mode (AsHandler)
import ExoMonad.Guest.Tool.Record (DispatchRecord (..), ReifyRecord (..))
import ExoMonad.Types (EventHandlerConfig, HookConfig (..), RoleConfig (..))

import qualified RootRole
import qualified TestrunnerRole

-- | Captured role capabilities (avoids existential field name restriction).
data RoleCapabilities = RoleCapabilities
  { rcDispatch :: Text -> Value -> IO (WasmResult MCPCallOutput),
    rcListTools :: [ToolDefinition],
    rcHooks :: HookConfig,
    rcEventHandlers :: EventHandlerConfig
  }

-- | Existential wrapper for role configs with heterogeneous tool types.
data SomeRoleConfig = SomeRoleConfig RoleCapabilities

-- | Smart constructor that captures capabilities from a typed RoleConfig.
mkSomeRoleConfig ::
  forall tools.
  (DispatchRecord tools, ReifyRecord tools) =>
  RoleConfig (tools AsHandler) ->
  SomeRoleConfig
mkSomeRoleConfig cfg =
  SomeRoleConfig $
    RoleCapabilities
      { rcDispatch = dispatchRecord (tools cfg),
        rcListTools = reifyToolDefs (Proxy @tools),
        rcHooks = hooks cfg,
        rcEventHandlers = eventHandlers cfg
      }

-- | Dispatch a tool call for this role.
roleDispatch :: SomeRoleConfig -> Text -> Value -> IO (WasmResult MCPCallOutput)
roleDispatch (SomeRoleConfig caps) = rcDispatch caps

-- | List tool definitions for this role.
roleListTools :: SomeRoleConfig -> [ToolDefinition]
roleListTools (SomeRoleConfig caps) = rcListTools caps

-- | List tool definitions in MCP format for this role.
roleToolsMCP :: SomeRoleConfig -> [Value]
roleToolsMCP = map toMCPFormat . roleListTools

-- | Get hook config for this role.
roleHooks :: SomeRoleConfig -> HookConfig
roleHooks (SomeRoleConfig caps) = rcHooks caps

-- | Get event handler config for this role.
roleEventHandlers :: SomeRoleConfig -> EventHandlerConfig
roleEventHandlers (SomeRoleConfig caps) = rcEventHandlers caps

-- | All role configs: root (PII rewriting) + testrunner.
allConfigs :: Map Text SomeRoleConfig
allConfigs =
  Map.fromList
    [ ("root", mkSomeRoleConfig RootRole.config),
      ("testrunner", mkSomeRoleConfig TestrunnerRole.config)
    ]

-- | Look up a role config by name.
lookupRole :: Text -> Maybe SomeRoleConfig
lookupRole = flip Map.lookup allConfigs
