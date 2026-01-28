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

-- | Generic dispatch of MCP tool calls to server and role handlers.
--
-- This module enables routing tool calls by name to the correct handler
-- function, all via Generic derivation.
--
-- = Server Dispatch
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
-- result <- dispatchServer handlerServer "spawn_agents" argsJson
-- -- Calls the spawnAgents handler with parsed args
-- @
--
-- = Role Dispatch
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
-- result <- dispatchRole handlerRole "spawn_agents" argsJson
-- -- Searches all servers for the tool and dispatches
-- @
module ExoMonad.Control.Role.Dispatch
  ( -- * Server Dispatch
    DispatchServer(..)
  , GDispatchServerFields(..)
  , DispatchResult(..)

    -- * Role Dispatch
  , DispatchRole(..)
  , GDispatchRoleFields(..)
  ) where

import Control.Monad.Freer (Eff)
import Data.Aeson (Value, FromJSON, ToJSON, fromJSON, toJSON)
import qualified Data.Aeson as A
import Data.Kind (Type)
import Data.Text (Text)
import GHC.Generics
import GHC.TypeLits (Symbol)

import ExoMonad.Control.Role.Types (ToolSpec(..), AsHandler, ToolHandler(..))

-- ════════════════════════════════════════════════════════════════════════════
-- DISPATCH RESULT
-- ════════════════════════════════════════════════════════════════════════════

-- | Result of attempting to dispatch a tool call.
data DispatchResult es
  = ToolNotFound
    -- ^ Tool name not found in server/role
  | ToolFound (Eff es Value)
    -- ^ Tool found; execute to get result

-- ════════════════════════════════════════════════════════════════════════════
-- SERVER DISPATCH
-- ════════════════════════════════════════════════════════════════════════════

-- | Typeclass for dispatching tool calls to server handlers.
--
-- @
-- result <- dispatchServer server "tool_name" argsJson
-- case result of
--   ToolNotFound -> error "unknown tool"
--   ToolFound action -> runM action
-- @
class DispatchServer (server :: Type -> [Type -> Type] -> Type) es where
  dispatchServer :: server (AsHandler es) es -> Text -> Value -> DispatchResult es

-- | Default instance using Generic derivation.
instance ( Generic (server (AsHandler es) es)
         , GDispatchServerFields (Rep (server (AsHandler es) es)) es
         )
      => DispatchServer server es where
  dispatchServer srv reqToolName args =
    gDispatchServerFields (from srv) reqToolName args

-- ════════════════════════════════════════════════════════════════════════════
-- GENERIC SERVER DISPATCH
-- ════════════════════════════════════════════════════════════════════════════

-- | Walk Generic Rep to find and dispatch to the correct tool handler.
class GDispatchServerFields (rep :: Type -> Type) es where
  gDispatchServerFields :: rep x -> Text -> Value -> DispatchResult es

-- Unwrap datatype metadata
instance GDispatchServerFields inner es => GDispatchServerFields (M1 D meta inner) es where
  gDispatchServerFields (M1 x) = gDispatchServerFields x

-- Unwrap constructor metadata
instance GDispatchServerFields inner es => GDispatchServerFields (M1 C meta inner) es where
  gDispatchServerFields (M1 x) = gDispatchServerFields x

-- Product: try left first, then right
instance (GDispatchServerFields left es, GDispatchServerFields right es)
      => GDispatchServerFields (left :*: right) es where
  gDispatchServerFields (l :*: r) reqToolName args =
    case gDispatchServerFields l reqToolName args of
      ToolFound action -> ToolFound action
      ToolNotFound -> gDispatchServerFields r reqToolName args

-- Empty: no fields
instance GDispatchServerFields U1 es where
  gDispatchServerFields U1 _ _ = ToolNotFound

-- Field: check if it's a tool handler that matches
instance GDispatchServerField name fieldType es
      => GDispatchServerFields (M1 S ('MetaSel ('Just name) su ss ds) (K1 i fieldType)) es where
  gDispatchServerFields (M1 (K1 val)) = gDispatchServerField @name val

-- ════════════════════════════════════════════════════════════════════════════
-- SERVER FIELD DISPATCH
-- ════════════════════════════════════════════════════════════════════════════

-- | Try to dispatch to a server field if it matches the tool name.
class GDispatchServerField (name :: Symbol) (fieldType :: Type) es where
  gDispatchServerField :: fieldType -> Text -> Value -> DispatchResult es

-- Tool handler field: check name match and dispatch
-- The field is a ToolHandler wrapper containing the handler function
-- We need ToolSpec to get the tool name, and FromJSON/ToJSON for args/result
instance {-# OVERLAPPABLE #-}
         ( ToolSpec tool
         , FromJSON (Args tool)
         , ToJSON (Result tool)
         )
      => GDispatchServerField name (ToolHandler es tool) es where
  gDispatchServerField (MkToolHandler handler) reqToolName args
    | toolName @tool == reqToolName =
        case fromJSON args of
          A.Error _msg -> ToolNotFound  -- Could return a ParseError variant
          A.Success parsedArgs ->
            ToolFound $ do
              result <- handler parsedArgs
              pure (toJSON result)
    | otherwise = ToolNotFound

-- Non-tool fields (description, etc.): skip
instance {-# OVERLAPPABLE #-} GDispatchServerField name fieldType es where
  gDispatchServerField _ _ _ = ToolNotFound

-- ════════════════════════════════════════════════════════════════════════════
-- ROLE DISPATCH
-- ════════════════════════════════════════════════════════════════════════════

-- | Typeclass for dispatching tool calls to role handlers.
--
-- Searches all servers in the role for the matching tool.
--
-- @
-- result <- dispatchRole role "tool_name" argsJson
-- case result of
--   ToolNotFound -> error "unknown tool"
--   ToolFound action -> runM action
-- @
class DispatchRole (roleRec :: Type -> [Type -> Type] -> Type) es where
  dispatchRole :: roleRec (AsHandler es) es -> Text -> Value -> DispatchResult es

-- | Default instance using Generic derivation.
instance ( Generic (roleRec (AsHandler es) es)
         , GDispatchRoleFields (Rep (roleRec (AsHandler es) es)) es
         )
      => DispatchRole roleRec es where
  dispatchRole r reqToolName args =
    gDispatchRoleFields (from r) reqToolName args

-- ════════════════════════════════════════════════════════════════════════════
-- GENERIC ROLE DISPATCH
-- ════════════════════════════════════════════════════════════════════════════

-- | Walk Generic Rep to find and dispatch to the correct server.
class GDispatchRoleFields (rep :: Type -> Type) es where
  gDispatchRoleFields :: rep x -> Text -> Value -> DispatchResult es

-- Unwrap datatype metadata
instance GDispatchRoleFields inner es => GDispatchRoleFields (M1 D meta inner) es where
  gDispatchRoleFields (M1 x) = gDispatchRoleFields x

-- Unwrap constructor metadata
instance GDispatchRoleFields inner es => GDispatchRoleFields (M1 C meta inner) es where
  gDispatchRoleFields (M1 x) = gDispatchRoleFields x

-- Product: try left first, then right
instance (GDispatchRoleFields left es, GDispatchRoleFields right es)
      => GDispatchRoleFields (left :*: right) es where
  gDispatchRoleFields (l :*: r) reqToolName args =
    case gDispatchRoleFields l reqToolName args of
      ToolFound action -> ToolFound action
      ToolNotFound -> gDispatchRoleFields r reqToolName args

-- Empty: no fields
instance GDispatchRoleFields U1 es where
  gDispatchRoleFields U1 _ _ = ToolNotFound

-- Field: check if it's a server and dispatch to it
instance GDispatchRoleField name fieldType es
      => GDispatchRoleFields (M1 S ('MetaSel ('Just name) su ss ds) (K1 i fieldType)) es where
  gDispatchRoleFields (M1 (K1 val)) = gDispatchRoleField @name val

-- ════════════════════════════════════════════════════════════════════════════
-- ROLE FIELD DISPATCH
-- ════════════════════════════════════════════════════════════════════════════

-- | Try to dispatch to a role field if it's a server.
class GDispatchRoleField (name :: Symbol) (fieldType :: Type) es where
  gDispatchRoleField :: fieldType -> Text -> Value -> DispatchResult es

-- Server field: dispatch to it
instance {-# OVERLAPPING #-}
         DispatchServer server es
      => GDispatchRoleField name (server (AsHandler es) es) es where
  gDispatchRoleField srv = dispatchServer srv

-- Non-server fields (metadata, hooks, etc.): skip
instance {-# OVERLAPPABLE #-} GDispatchRoleField name fieldType es where
  gDispatchRoleField _ _ _ = ToolNotFound
