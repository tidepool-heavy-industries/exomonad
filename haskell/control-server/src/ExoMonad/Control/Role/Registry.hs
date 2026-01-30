{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoStarIsType #-}

-- | Role Registry - Bridge between Role DSL and runtime.
module ExoMonad.Control.Role.Registry
  ( -- * Role Schema Access
    roleSchemaFor,
    allToolsForRole,
    isToolAllowedTyped,

    -- * Re-exports
    RoleSchema (..),
  )
where

import Data.Set qualified as Set
import Data.Text (Text)
-- Role imports

import ExoMonad.Control.Role.Definition.Dev (DevRole (..), devMetadata)
import ExoMonad.Control.Role.Definition.PM (PMRole (..), pmMetadata)
import ExoMonad.Control.Role.Definition.TL (TLRole (..), tlMetadata)
-- Tool imports

import ExoMonad.Control.Role.Hook.Definitions
import ExoMonad.Control.Role.Reify
  ( ReifyRole (..),
    RoleSchema (..),
  )
import ExoMonad.Control.Role.Schema (AsSchema, hookSchema, reifyTools, toolSchema)
import ExoMonad.Control.Role.Tool.Definitions
import ExoMonad.Graph.MCPReify (MCPToolInfo (..))
import ExoMonad.Role (Role (..))

-- ════════════════════════════════════════════════════════════════════════════
-- SCHEMA VALUES
-- ════════════════════════════════════════════════════════════════════════════

-- Shared Tool Sets
orchestrationSchema :: OrchestrationTools AsSchema
orchestrationSchema =
  OrchestrationTools
    { spawnAgents = toolSchema,
      exoStatus = toolSchema
    }

tuiSchema :: TUITools AsSchema
tuiSchema =
  TUITools
    { popup = toolSchema
    }

gitHubSchema :: GitHubTools AsSchema
gitHubSchema =
  GitHubTools
    { ghIssueList = toolSchema,
      ghIssueShow = toolSchema
    }

kaizenSchema :: KaizenTools AsSchema
kaizenSchema =
  KaizenTools
    { kaizenReport = toolSchema
    }

-- Hook Schemas
commonHooksSchema :: CommonHooks AsSchema
commonHooksSchema =
  CommonHooks
    { sessionStart = hookSchema,
      preToolUse = hookSchema,
      postToolUse = hookSchema,
      stop = hookSchema,
      sessionEnd = hookSchema,
      notification = hookSchema,
      subagentStop = hookSchema
    }

gitToolsSchema :: GitTools AsSchema
gitToolsSchema =
  GitTools
    { filePR = toolSchema
    }

pmEpicToolsSchema :: PMEpicTools AsSchema
pmEpicToolsSchema =
  PMEpicTools
    { pmEpicCreate = toolSchema,
      pmEpicList = toolSchema,
      pmEpicUpdate = toolSchema
    }

pmStrategyToolsSchema :: PMStrategyTools AsSchema
pmStrategyToolsSchema =
  PMStrategyTools
    { pmPitch = toolSchema,
      pmInterview = toolSchema
    }

-- Role Tool Sets
tlToolsSchema :: TLTools AsSchema
tlToolsSchema =
  TLTools
    { orchestration = orchestrationSchema,
      tui = tuiSchema,
      github = gitHubSchema,
      kaizen = kaizenSchema,
      specific = TLSpecificTools {tlCreateIssue = toolSchema}
    }

devToolsSchema :: DevTools AsSchema
devToolsSchema =
  DevTools
    { tui = tuiSchema,
      github = gitHubSchema,
      kaizen = kaizenSchema,
      git = GitTools {filePR = toolSchema}
    }

pmToolsSchema :: PMTools AsSchema
pmToolsSchema =
  PMTools
    { tui = tuiSchema,
      github = gitHubSchema,
      kaizen = kaizenSchema,
      epic = pmEpicToolsSchema,
      strategy = pmStrategyToolsSchema,
      specific = PMSpecificTools {pmStatus = toolSchema}
    }

-- Role Schemas
tlRoleSchema :: TLRole AsSchema
tlRoleSchema =
  TLRole
    { tlToolsRecord = tlToolsSchema,
      tlMetadata = tlMetadata,
      tlHooks = TLHooks {common = commonHooksSchema}
    }

devRoleSchema :: DevRole AsSchema
devRoleSchema =
  DevRole
    { devToolsRecord = devToolsSchema,
      devMetadata = devMetadata,
      devHooks = DevHooks {common = commonHooksSchema}
    }

pmRoleSchema :: PMRole AsSchema
pmRoleSchema =
  PMRole
    { pmToolsRecord = pmToolsSchema,
      pmMetadata = pmMetadata,
      pmHooks = PMHooks {common = commonHooksSchema}
    }

-- ════════════════════════════════════════════════════════════════════════════
-- REIFICATION INSTANCES
-- ════════════════════════════════════════════════════════════════════════════

instance ReifyRole TLRole where
  reifyRole role =
    RoleSchema
      { metadata = role.tlMetadata,
        tools = reifyTools role.tlToolsRecord
      }

instance ReifyRole DevRole where
  reifyRole role =
    RoleSchema
      { metadata = role.devMetadata,
        tools = reifyTools role.devToolsRecord
      }

instance ReifyRole PMRole where
  reifyRole role =
    RoleSchema
      { metadata = role.pmMetadata,
        tools = reifyTools role.pmToolsRecord
      }

-- ════════════════════════════════════════════════════════════════════════════
-- PUBLIC API
-- ════════════════════════════════════════════════════════════════════════════

-- | Get the schema for a role.
roleSchemaFor :: Role -> RoleSchema
roleSchemaFor TL = reifyRole tlRoleSchema
roleSchemaFor Dev = reifyRole devRoleSchema
roleSchemaFor PM = reifyRole pmRoleSchema

-- | Get all tool names available for a role.
allToolsForRole :: Role -> Set.Set Text
allToolsForRole r =
  let schema = roleSchemaFor r
   in Set.fromList $ map getName schema.tools
  where
    getName (MCPToolInfo {mtdName = name}) = name

-- | Check if a tool is allowed for a role (typed version).
isToolAllowedTyped :: Role -> Text -> Bool
isToolAllowedTyped r tName = Set.member tName (allToolsForRole r)
