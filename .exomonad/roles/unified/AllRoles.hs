{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | Unified role registry for the HTTP-native single-WASM architecture.
--
-- Instead of separate WASM modules per role, this module exports the union
-- of all role configs. The Rust server calls WASM entry points with a role
-- parameter and dispatches to the correct config.
--
-- Individual Role.hs files remain unchanged — they are imported here.
module AllRoles
  ( allConfigs,
    lookupRole,
    SomeRoleConfig (..),

    -- * Accessors
    roleDispatch,
    roleListTools,
    roleHooks,
    roleToolsMCP,
  )
where

import Data.Aeson (Value)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import ExoMonad.Guest.Tool.Class (MCPCallOutput, ToolDefinition, toMCPFormat)
import ExoMonad.Guest.Tool.Mode (AsHandler)
import ExoMonad.Guest.Tool.Record (DispatchRecord (..), ReifyRecord (..))
import ExoMonad.Types (HookConfig (..), RoleConfig (..))

-- Import both role configs under unique module names.
import qualified DevRole
import qualified TLRole

-- | Captured role capabilities (avoids existential field name restriction).
data RoleCapabilities = RoleCapabilities
  { -- | Dispatch a tool call by name through this role's handlers.
    rcDispatch :: Text -> Value -> IO MCPCallOutput,
    -- | List all tool definitions for this role (raw).
    rcListTools :: [ToolDefinition],
    -- | Get hook config for this role.
    rcHooks :: HookConfig
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
        rcHooks = hooks cfg
      }

-- | Dispatch a tool call for this role.
roleDispatch :: SomeRoleConfig -> Text -> Value -> IO MCPCallOutput
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

-- | All role configs indexed by role name.
allConfigs :: Map Text SomeRoleConfig
allConfigs =
  Map.fromList
    [ ("tl", mkSomeRoleConfig TLRole.config),
      ("dev", mkSomeRoleConfig DevRole.config)
    ]

-- | Look up a role config by name.
lookupRole :: Text -> Maybe SomeRoleConfig
lookupRole = flip Map.lookup allConfigs
