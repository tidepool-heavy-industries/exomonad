{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}

-- | Project Manager Role Definition.
--
-- The PM role handles planning, prioritization, and health monitoring.
module ExoMonad.Control.Role.Definition.PM
  ( PMRole(..)
  , pmMetadata
  ) where

import Data.Kind (Type)
import GHC.Generics (Generic)

import ExoMonad.Control.Role.Types
  ( Hooks
  , RoleMetadata(..)
  , ServerField
  )
import ExoMonad.Control.Role.Server.Planning (PlanningServer)
import ExoMonad.Control.Role.Server.TUI (TUIServer)
import ExoMonad.Control.Role.Server.GitHub (GitHubServer)
import ExoMonad.Control.Role.Server.Kaizen (KaizenServer)

-- | Project Manager role record.
data PMRole mode (es :: [Type -> Type]) = PMRole
  { pmMetadataField :: RoleMetadata
  , pmHooks         :: Hooks es
  , pmPlanning      :: ServerField mode es PlanningServer
  , pmTUI           :: ServerField mode es TUIServer
  , pmGitHub        :: ServerField mode es GitHubServer
  , pmKaizen        :: ServerField mode es KaizenServer
  }
  deriving Generic

-- | PM role metadata.
pmMetadata :: RoleMetadata
pmMetadata = RoleMetadata
  { rmSlug = "pm"
  , rmDisplayName = "Project Manager"
  , rmDescription = "Planning agent: prioritizes work, monitors sprint health, approves expansions"
  }
