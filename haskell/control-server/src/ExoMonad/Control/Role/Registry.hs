{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoStarIsType #-}

-- | Role Registry - Bridge between Role DSL and runtime.
--
-- This module provides typed access to role definitions and their schemas.
-- It serves as the glue between the declarative Role DSL and the
-- imperative handler code.
--
-- = Usage
--
-- @
-- -- Get schema for a role (for MCP tool discovery)
-- let schema = roleSchemaFor TL
--
-- -- Check if a tool is available for a role
-- let allowed = isToolAllowedTyped TL "spawn_agents"
-- @
module ExoMonad.Control.Role.Registry
  ( -- * Role Schema Access
    roleSchemaFor
  , allToolsForRole
  , isToolAllowedTyped

    -- * Re-exports
  , RoleSchema(..)
  , ServerSchema(..)
  ) where

import Data.Text (Text)
import qualified Data.Set as Set
import Data.Aeson (Value(..))

import ExoMonad.Role (Role(..))
import ExoMonad.Graph.MCPReify (MCPToolInfo(..))
import ExoMonad.Schema (jsonSchema, schemaToValue)
import ExoMonad.Control.Role.Types
  ( AsSchema
  , emptyHooks
  , ToolSpec(..)
  )
import ExoMonad.Control.Role.Reify
  ( RoleSchema(..)
  , ServerSchema(..)
  , ReifyRole(..)
  )
-- Server imports
import ExoMonad.Control.Role.Server.Orchestration (OrchestrationServer(..))
import ExoMonad.Control.Role.Server.TUI (TUIServer(..))
import ExoMonad.Control.Role.Server.Workflow (WorkflowServer(..))
import ExoMonad.Control.Role.Server.Planning (PlanningServer(..))
import ExoMonad.Control.Role.Server.GitHub (GitHubServer(..))
import ExoMonad.Control.Role.Server.TL (TLServer(..))
import ExoMonad.Control.Role.Server.Kaizen (KaizenServer(..))
-- Role imports
import ExoMonad.Control.Role.Definition.TL (TLRole(..), tlMetadata)
import ExoMonad.Control.Role.Definition.Dev (DevRole(..), devMetadata)
import ExoMonad.Control.Role.Definition.PM (PMRole(..), pmMetadata)
-- Tool type imports for MCPToolInfo construction
import ExoMonad.Control.Role.Tool.SpawnAgents (SpawnAgents)
import ExoMonad.Control.Role.Tool.ExoStatus (ExoStatus)
import ExoMonad.Control.Role.Tool.Popup (Popup)
import ExoMonad.Control.Role.Tool.FilePR (FilePR)
import ExoMonad.Control.Role.Tool.PMStatus (PMStatus)
import ExoMonad.Control.Role.Tool.GitHub (GHIssueList, GHIssueShow)
import ExoMonad.Control.Role.Tool.TLCreateIssue (TLCreateIssue)
import ExoMonad.Control.Role.Tool.KaizenReport (KaizenReport)

-- ════════════════════════════════════════════════════════════════════════════
-- SCHEMA VALUES
-- ════════════════════════════════════════════════════════════════════════════

-- | Tool info helper - creates MCPToolInfo from ToolSpec.
toolInfo :: forall tool. ToolSpec tool => MCPToolInfo
toolInfo = MCPToolInfo
  { mtdName = toolName @tool
  , mtdDescription = toolDescription @tool
  , mtdInputSchema = schemaToValue (jsonSchema @(Args tool))
  , mtdEntryName = toolName @tool   -- Same as tool name for Role DSL tools
  , mtdRoles = []                   -- Roles determined by role definition, not tool
  }

-- | Orchestration server schema.
orchestrationSchema :: OrchestrationServer AsSchema es
orchestrationSchema = OrchestrationServer
  { osDescription = "Agent orchestration tools: spawn workers, monitor status"
  , osSpawnAgents = toolInfo @SpawnAgents
  , osExoStatus = toolInfo @ExoStatus
  }

-- | TUI server schema.
tuiSchema :: TUIServer AsSchema es
tuiSchema = TUIServer
  { tuiDescription = "Interactive UI tools"
  , tuiPopup = toolInfo @Popup
  }

-- | Workflow server schema.
workflowSchema :: WorkflowServer AsSchema es
workflowSchema = WorkflowServer
  { wfDescription = "Developer workflow tools"
  , wfFilePR = toolInfo @FilePR
  }

-- | Planning server schema.
planningSchema :: PlanningServer AsSchema es
planningSchema = PlanningServer
  { plDescription = "Project management tools"
  , plPMStatus = toolInfo @PMStatus
  }

-- | GitHub server schema.
gitHubSchema :: GitHubServer AsSchema es
gitHubSchema = GitHubServer
  { ghDescription = "GitHub issue management"
  , ghIssueList = toolInfo @GHIssueList
  , ghIssueShow = toolInfo @GHIssueShow
  }

-- | TL server schema.
tlServerSchema :: TLServer AsSchema es
tlServerSchema = TLServer
  { tlCreateIssue = toolInfo @TLCreateIssue
  }

-- | Kaizen server schema.
kaizenSchema :: KaizenServer AsSchema es
kaizenSchema = KaizenServer
  { kzDescription = "Continuous improvement and feedback tools"
  , kzReport = toolInfo @KaizenReport
  }

-- ════════════════════════════════════════════════════════════════════════════
-- ROLE SCHEMAS
-- ════════════════════════════════════════════════════════════════════════════

-- | TL role schema value.
tlRoleSchema :: TLRole AsSchema es
tlRoleSchema = TLRole
  { tlMetadataField = tlMetadata
  , tlHooks = emptyHooks
  , tlOrchestration = orchestrationSchema
  , tlTUI = tuiSchema
  , tlGitHub = gitHubSchema
  , tlTools = tlServerSchema
  , tlKaizen = kaizenSchema
  }

-- | Dev role schema value.
devRoleSchema :: DevRole AsSchema es
devRoleSchema = DevRole
  { devMetadataField = devMetadata
  , devHooks = emptyHooks
  , devWorkflow = workflowSchema
  , devTUI = tuiSchema
  , devGitHub = gitHubSchema
  , devKaizen = kaizenSchema
  }

-- | PM role schema value.
pmRoleSchema :: PMRole AsSchema es
pmRoleSchema = PMRole
  { pmMetadataField = pmMetadata
  , pmHooks = emptyHooks
  , pmPlanning = planningSchema
  , pmTUI = tuiSchema
  , pmGitHub = gitHubSchema
  , pmKaizen = kaizenSchema
  }

-- ════════════════════════════════════════════════════════════════════════════
-- PUBLIC API
-- ════════════════════════════════════════════════════════════════════════════

-- | Get the schema for a role.
--
-- This extracts tool metadata from the typed role definition,
-- which can be used for MCP tool discovery.
roleSchemaFor :: Role -> RoleSchema
roleSchemaFor TL = reifyRole tlRoleSchema
roleSchemaFor Dev = reifyRole devRoleSchema
roleSchemaFor PM = reifyRole pmRoleSchema

-- | Get all tool names available for a role.
allToolsForRole :: Role -> Set.Set Text
allToolsForRole r =
  let schema = roleSchemaFor r
      serverTools = concatMap (\(_, srv) -> srv.tools) (schema.servers)
  in Set.fromList $ map (.mtdName) serverTools

-- | Check if a tool is allowed for a role (typed version).
--
-- This replaces the stringly-typed isToolAllowed in RoleConfig.
isToolAllowedTyped :: Role -> Text -> Bool
isToolAllowedTyped r tName = Set.member tName (allToolsForRole r)
