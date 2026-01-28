{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}

-- | Team Lead (TL) Role Definition.
--
-- The TL role is the primary orchestrator with full access to all tools.
-- It can spawn agents, monitor status, and coordinate across the team.
--
-- = Structure
--
-- @
-- TLRole
-- ├── metadata: { slug = "tl", displayName = "Team Lead", ... }
-- ├── hooks: { sessionStart = ..., stop = ..., ... }
-- └── orchestration: OrchestrationServer
--     ├── spawn_agents
--     └── exo_status
-- @
--
-- = Usage
--
-- @
-- -- Schema mode: for MCP registration
-- tlSchema :: TLRole AsSchema es
-- tlSchema = tlRole AsSchema
--
-- -- Handler mode: for dispatch
-- tlHandlers :: Member Git es => TLRole (AsHandler es) es
-- tlHandlers = tlRole AsHandler
-- @
module ExoMonad.Control.Role.Definition.TL
  ( TLRole(..)
  , tlMetadata
  ) where

import Data.Kind (Type)
import GHC.Generics (Generic)

import ExoMonad.Control.Role.Types
  ( Hooks
  , RoleMetadata(..)
  , ServerField
  )
import ExoMonad.Control.Role.Server.Orchestration (OrchestrationServer)
import ExoMonad.Control.Role.Server.TUI (TUIServer)
import ExoMonad.Control.Role.Server.GitHub (GitHubServer)
import ExoMonad.Control.Role.Server.TL (TLServer)
import ExoMonad.Control.Role.Server.Kaizen (KaizenServer)

-- | Team Lead role record.
--
-- In 'AsSchema' mode, server fields become 'ServerSchema' for registration.
-- In 'AsHandler' mode, server fields become servers with handler functions.
data TLRole mode (es :: [Type -> Type]) = TLRole
  { tlMetadataField  :: RoleMetadata
    -- ^ Role identity
  , tlHooks         :: Hooks es
    -- ^ Lifecycle hooks
  , tlOrchestration :: ServerField mode es OrchestrationServer
    -- ^ Orchestration tools (spawn_agents, exo_status)
  , tlTUI           :: ServerField mode es TUIServer
    -- ^ TUI tools (popup)
  , tlGitHub        :: ServerField mode es GitHubServer
    -- ^ GitHub tools (gh_issue_list, gh_issue_show)
  , tlTools         :: ServerField mode es TLServer
    -- ^ TL-specific tools (tl_create_issue)
  , tlKaizen        :: ServerField mode es KaizenServer
    -- ^ Kaizen tools (kaizen_report)
  }
  deriving Generic

-- | Metadata for the TL role.
tlMetadata :: RoleMetadata
tlMetadata = RoleMetadata
  { rmSlug = "tl"
  , rmDisplayName = "Team Lead"
  , rmDescription = "Primary orchestrator with full access to spawn agents, monitor status, and coordinate team work."
  }
