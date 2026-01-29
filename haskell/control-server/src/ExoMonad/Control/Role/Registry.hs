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
    roleSchemaFor
  , allToolsForRole
  , isToolAllowedTyped

    -- * Re-exports
  , RoleSchema(..)
  ) where

import Data.Text (Text)
import qualified Data.Set as Set

import ExoMonad.Role (Role(..))
import ExoMonad.Control.Role.Types
  ( emptyHooks
  )
import ExoMonad.Control.Role.Reify
  ( RoleSchema(..)
  , ReifyRole(..)
  )
import ExoMonad.Control.Role.Schema (toolSchema, reifyTools, AsSchema)
import ExoMonad.Graph.MCPReify (MCPToolInfo(..))

-- Role imports
import ExoMonad.Control.Role.Definition.TL (TLRole(..), tlMetadata)
import ExoMonad.Control.Role.Definition.Dev (DevRole(..), devMetadata)
import ExoMonad.Control.Role.Definition.PM (PMRole(..), pmMetadata)

-- Tool imports
import ExoMonad.Control.Role.Tool.Definitions

-- ════════════════════════════════════════════════════════════════════════════
-- SCHEMA VALUES
-- ════════════════════════════════════════════════════════════════════════════

-- Shared Tool Sets
orchestrationSchema :: OrchestrationTools AsSchema
orchestrationSchema = OrchestrationTools
  { spawnAgents = toolSchema
  , exoStatus   = toolSchema
  }

tuiSchema :: TUITools AsSchema
tuiSchema = TUITools
  { popup = toolSchema
  }

gitHubSchema :: GitHubTools AsSchema
gitHubSchema = GitHubTools
  { ghIssueList = toolSchema
  , ghIssueShow = toolSchema
  }

kaizenSchema :: KaizenTools AsSchema
kaizenSchema = KaizenTools
  { kaizenReport = toolSchema
  }

-- Role Tool Sets
tlToolsSchema :: TLTools AsSchema
tlToolsSchema = TLTools
  { orchestration = orchestrationSchema
  , tui           = tuiSchema
  , github        = gitHubSchema
  , kaizen        = kaizenSchema
  , specific      = TLSpecificTools { tlCreateIssue = toolSchema }
  }

devToolsSchema :: DevTools AsSchema
devToolsSchema = DevTools
  { orchestration = orchestrationSchema
  , tui           = tuiSchema
  , github        = gitHubSchema
  , kaizen        = kaizenSchema
  , specific      = DevSpecificTools { filePR = toolSchema }
  }

pmToolsSchema :: PMTools AsSchema
pmToolsSchema = PMTools
  { orchestration = orchestrationSchema
  , tui           = tuiSchema
  , github        = gitHubSchema
  , kaizen        = kaizenSchema
  , specific      = PMSpecificTools { pmStatus = toolSchema }
  }

-- Role Schemas
tlRoleSchema :: TLRole AsSchema es
tlRoleSchema = TLRole
  { tlToolsRecord = tlToolsSchema
  , tlMetadata    = tlMetadata
  , tlHooks       = emptyHooks
  }

devRoleSchema :: DevRole AsSchema es
devRoleSchema = DevRole
  { devToolsRecord = devToolsSchema
  , devMetadata    = devMetadata
  , devHooks       = emptyHooks
  }

pmRoleSchema :: PMRole AsSchema es
pmRoleSchema = PMRole
  { pmToolsRecord = pmToolsSchema
  , pmMetadata    = pmMetadata
  , pmHooks       = emptyHooks
  }

-- ════════════════════════════════════════════════════════════════════════════
-- REIFICATION INSTANCES
-- ════════════════════════════════════════════════════════════════════════════

instance ReifyRole TLRole es where
  reifyRole role = RoleSchema
    { metadata = role.tlMetadata
    , tools    = reifyTools role.tlToolsRecord
    }

instance ReifyRole DevRole es where
  reifyRole role = RoleSchema
    { metadata = role.devMetadata
    , tools    = reifyTools role.devToolsRecord
    }

instance ReifyRole PMRole es where
  reifyRole role = RoleSchema
    { metadata = role.pmMetadata
    , tools    = reifyTools role.pmToolsRecord
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
    getName (MCPToolInfo { mtdName = name }) = name

-- | Check if a tool is allowed for a role (typed version).
isToolAllowedTyped :: Role -> Text -> Bool
isToolAllowedTyped r tName = Set.member tName (allToolsForRole r)
