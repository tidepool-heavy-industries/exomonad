{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

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

import ExoMonad.Control.Role.Types (RoleMetadata(..))
import ExoMonad.Control.Role.Tool.Definitions (TLTools)
import ExoMonad.Control.Role.Hook.Definitions (TLHooks)

-- | Team Lead role record.
data TLRole mode = TLRole
  { tlToolsRecord :: TLTools mode
    -- ^ Tool definitions
  , tlMetadata    :: RoleMetadata
    -- ^ Role identity
  , tlHooks       :: TLHooks mode
    -- ^ Lifecycle hooks
  }
  deriving Generic

-- | Metadata for the TL role.
tlMetadata :: RoleMetadata
tlMetadata = RoleMetadata
  { rmSlug = "tl"
  , rmDisplayName = "Team Lead"
  , rmDescription = "Primary orchestrator with full access to spawn agents, monitor status, and coordinate team work."
  }
