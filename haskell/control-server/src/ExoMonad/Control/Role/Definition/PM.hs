{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Project Manager Role Definition.
--
-- The PM role handles planning, prioritization, and health monitoring.
module ExoMonad.Control.Role.Definition.PM
  ( PMRole(..)
  , pmMetadata
  ) where

import Data.Kind (Type)
import GHC.Generics (Generic)

import ExoMonad.Control.Role.Types (RoleMetadata(..))
import ExoMonad.Control.Role.Tool.Definitions (PMTools)
import ExoMonad.Control.Role.Hook.Definitions (PMHooks)

-- | Project Manager role record.
data PMRole mode = PMRole
  { pmToolsRecord :: PMTools mode
  , pmMetadata    :: RoleMetadata
  , pmHooks       :: PMHooks mode
  }
  deriving Generic

-- | PM role metadata.
pmMetadata :: RoleMetadata
pmMetadata = RoleMetadata
  { rmSlug = "pm"
  , rmDisplayName = "Project Manager"
  , rmDescription = "Planning agent: prioritizes work, monitors sprint health, approves expansions"
  }
