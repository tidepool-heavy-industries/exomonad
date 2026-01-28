{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Generic reification of server and role schemas from mode-parameterized records.
--
-- This module enables extracting MCP tool definitions from server records
-- and aggregating them into role schemas, all via Generic derivation.
--
-- = Server Reification
--
-- @
-- data OrchestrationServer mode es = OrchestrationServer
--   { description :: Text
--   , spawnAgents :: ToolField mode es SpawnAgents
--   , exoStatus   :: ToolField mode es ExoStatus
--   }
--   deriving Generic
--
-- -- At runtime:
-- schema <- reifyServer (schemaServer :: OrchestrationServer AsSchema es)
-- -- ServerSchema { ssDescription = "...", ssTools = [MCPToolInfo "spawn_agents" ..., ...] }
-- @
--
-- = Role Reification
--
-- @
-- data TLRole mode es = TLRole
--   { metadata      :: RoleMetadata
--   , hooks         :: Hooks es
--   , orchestration :: ServerField mode es OrchestrationServer
--   , messaging     :: ServerField mode es MessagingServer
--   }
--   deriving Generic
--
-- -- At runtime:
-- roleSchema <- reifyRole (schemaRole :: TLRole AsSchema es)
-- -- RoleSchema { rsMetadata = ..., rsServers = [("orchestration", ...), ("messaging", ...)] }
-- @
module ExoMonad.Control.Role.Reify
  ( -- * Server Schema
    ServerSchema(..)
  , ReifyServer(..)
  , GReifyServerFields(..)

    -- * Role Schema
  , RoleSchema(..)
  , ReifyRole(..)
  , GReifyRoleFields(..)
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)

import ExoMonad.Graph.MCPReify (MCPToolInfo(..))
import ExoMonad.Control.Role.Types (RoleMetadata, AsSchema)

-- ════════════════════════════════════════════════════════════════════════════
-- SERVER SCHEMA
-- ════════════════════════════════════════════════════════════════════════════

-- | Schema for an MCP server, extracted from a server record in AsSchema mode.
data ServerSchema = ServerSchema
  { description :: Text
    -- ^ Human-readable server description
  , tools :: [MCPToolInfo]
    -- ^ List of tools provided by this server
  } deriving (Show, Eq, Generic)

-- | Typeclass for server records that can be reified to schemas.
--
-- Server records must have:
-- - A 'description :: Text' field
-- - Zero or more tool fields of type 'ToolField AsSchema es SomeTool'
--
-- @
-- instance (Generic (server AsSchema es), GReifyServerFields (Rep (server AsSchema es)))
--       => ReifyServer server es where
--   reifyServer srv = ...
-- @
class ReifyServer (server :: Type -> [Type -> Type] -> Type) (es :: [Type -> Type]) where
  reifyServer :: server AsSchema es -> ServerSchema

-- | Default instance using Generic derivation.
instance ( Generic (server AsSchema es)
         , GReifyServerFields (Rep (server AsSchema es))
         )
      => ReifyServer server es where
  reifyServer srv =
    let (desc, toolList) = gReifyServerFields (from srv)
    in ServerSchema
      { description = desc
      , tools = toolList
      }

-- ════════════════════════════════════════════════════════════════════════════
-- GENERIC SERVER TRAVERSAL
-- ════════════════════════════════════════════════════════════════════════════

-- | Walk Generic Rep collecting server description and tools.
--
-- Returns (description, tools) where description is empty if not found.
class GReifyServerFields (rep :: Type -> Type) where
  gReifyServerFields :: rep x -> (Text, [MCPToolInfo])

-- Unwrap datatype metadata
instance GReifyServerFields inner => GReifyServerFields (M1 D meta inner) where
  gReifyServerFields (M1 x) = gReifyServerFields x

-- Unwrap constructor metadata
instance GReifyServerFields inner => GReifyServerFields (M1 C meta inner) where
  gReifyServerFields (M1 x) = gReifyServerFields x

-- Product: combine both sides
instance (GReifyServerFields left, GReifyServerFields right)
      => GReifyServerFields (left :*: right) where
  gReifyServerFields (l :*: r) =
    let (descL, toolsL) = gReifyServerFields l
        (descR, toolsR) = gReifyServerFields r
        -- Use non-empty description (left takes precedence)
        desc = if T.null descL then descR else descL
    in (desc, toolsL ++ toolsR)

-- Empty: no fields
instance GReifyServerFields U1 where
  gReifyServerFields U1 = ("", [])

-- Field: check if it's description, MCPToolInfo, or something else
instance GReifyServerField name fieldType
      => GReifyServerFields (M1 S ('MetaSel ('Just name) su ss ds) (K1 i fieldType)) where
  gReifyServerFields (M1 (K1 val)) = gReifyServerField @name val

-- ════════════════════════════════════════════════════════════════════════════
-- FIELD CLASSIFICATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Classify and extract data from a server field.
class GReifyServerField (name :: Symbol) (fieldType :: Type) where
  gReifyServerField :: fieldType -> (Text, [MCPToolInfo])

-- Description field: extract the text
instance {-# OVERLAPPING #-} GReifyServerField "description" Text where
  gReifyServerField desc = (desc, [])

-- MCPToolInfo field: collect it
instance {-# OVERLAPPING #-} GReifyServerField name MCPToolInfo where
  gReifyServerField tool = ("", [tool])

-- Other fields (including Hooks, RoleMetadata, etc.): ignore
instance {-# OVERLAPPABLE #-} GReifyServerField name fieldType where
  gReifyServerField _ = ("", [])

-- ════════════════════════════════════════════════════════════════════════════
-- ROLE SCHEMA
-- ════════════════════════════════════════════════════════════════════════════

-- | Schema for a role, extracted from a role record in AsSchema mode.
data RoleSchema = RoleSchema
  { metadata :: RoleMetadata
    -- ^ Role identity information
  , servers :: [(Text, ServerSchema)]
    -- ^ Named servers belonging to this role
  } deriving (Show, Eq, Generic)

-- | Typeclass for role records that can be reified to schemas.
--
-- Role records must have:
-- - A 'metadata :: RoleMetadata' field
-- - Zero or more server fields of type 'ServerField mode es SomeServer'
--
-- @
-- instance (Generic (role AsSchema es), GReifyRoleFields (Rep (role AsSchema es)))
--       => ReifyRole role es where
--   reifyRole role = ...
-- @
class ReifyRole (roleRec :: Type -> [Type -> Type] -> Type) (es :: [Type -> Type]) where
  reifyRole :: roleRec AsSchema es -> RoleSchema

-- | Default instance using Generic derivation.
instance ( Generic (roleRec AsSchema es)
         , GReifyRoleFields (Rep (roleRec AsSchema es))
         )
      => ReifyRole roleRec es where
  reifyRole r =
    let (meta, serverList) = gReifyRoleFields (from r)
    in RoleSchema
      { metadata = meta
      , servers = serverList
      }

-- ════════════════════════════════════════════════════════════════════════════
-- GENERIC ROLE TRAVERSAL
-- ════════════════════════════════════════════════════════════════════════════

-- | Walk Generic Rep collecting role metadata and servers.
--
-- Returns (metadata, servers) where metadata must be provided.
class GReifyRoleFields (rep :: Type -> Type) where
  gReifyRoleFields :: rep x -> (RoleMetadata, [(Text, ServerSchema)])

-- Unwrap datatype metadata
instance GReifyRoleFields inner => GReifyRoleFields (M1 D meta inner) where
  gReifyRoleFields (M1 x) = gReifyRoleFields x

-- Unwrap constructor metadata
instance GReifyRoleFields inner => GReifyRoleFields (M1 C meta inner) where
  gReifyRoleFields (M1 x) = gReifyRoleFields x

-- Product: combine both sides
instance (GReifyRoleFields left, GReifyRoleFields right)
      => GReifyRoleFields (left :*: right) where
  gReifyRoleFields (l :*: r) =
    let (metaL, serversL) = gReifyRoleFields l
        (_metaR, serversR) = gReifyRoleFields r
        -- Use non-default metadata (left takes precedence)
        -- This is a bit hacky - we assume at least one side has the real metadata
    in (metaL, serversL ++ serversR)

-- Empty: no fields
instance GReifyRoleFields U1 where
  gReifyRoleFields U1 = (defaultMetadata, [])
    where
      defaultMetadata = error "Role record missing 'metadata' field"

-- Field: check if it's metadata, a server, or something else
instance GReifyRoleField name fieldType
      => GReifyRoleFields (M1 S ('MetaSel ('Just name) su ss ds) (K1 i fieldType)) where
  gReifyRoleFields (M1 (K1 val)) = gReifyRoleField @name val

-- ════════════════════════════════════════════════════════════════════════════
-- ROLE FIELD CLASSIFICATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Classify and extract data from a role field.
class GReifyRoleField (name :: Symbol) (fieldType :: Type) where
  gReifyRoleField :: fieldType -> (RoleMetadata, [(Text, ServerSchema)])

-- Metadata field: extract it
instance {-# OVERLAPPING #-} GReifyRoleField "metadata" RoleMetadata where
  gReifyRoleField meta = (meta, [])

-- Server field: reify it and include the field name
-- This requires the server type to have a ReifyServer instance
instance {-# OVERLAPPING #-}
         ( KnownSymbol name
         , ReifyServer server es
         )
      => GReifyRoleField name (server AsSchema es) where
  gReifyRoleField srv =
    let schema = reifyServer srv
        fieldName = T.pack (symbolVal (Proxy @name))
    in (placeholderMetadata, [(fieldName, schema)])
    where
      placeholderMetadata = error "accessing placeholder metadata"

-- Other fields (Hooks, etc.): ignore
instance {-# OVERLAPPABLE #-} GReifyRoleField name fieldType where
  gReifyRoleField _ = (placeholderMetadata, [])
    where
      placeholderMetadata = error "accessing placeholder metadata"
