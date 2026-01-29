{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | Role Reification.
--
-- Extracts RoleSchema from role records.
module ExoMonad.Control.Role.Reify
  ( RoleSchema(..)
  , ReifyRole(..)
  ) where

import Data.Kind (Type)
import ExoMonad.Graph.MCPReify (MCPToolInfo)
import ExoMonad.Control.Role.Types (RoleMetadata)
import ExoMonad.Control.Role.Schema (AsSchema)

-- | Schema for a role.
data RoleSchema = RoleSchema
  { metadata :: RoleMetadata
  , tools    :: [MCPToolInfo]
  } deriving (Show, Eq)

-- | Typeclass for role records that can be reified.
class ReifyRole (roleRec :: Type -> [Type -> Type] -> Type) (es :: [Type -> Type]) where
  reifyRole :: roleRec AsSchema es -> RoleSchema