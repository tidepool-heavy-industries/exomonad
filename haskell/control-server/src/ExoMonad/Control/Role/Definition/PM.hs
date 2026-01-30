{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}

-- | Project Manager Role Definition.
--
-- The PM role handles planning, prioritization, and health monitoring.
module ExoMonad.Control.Role.Definition.PM
  ( PMRole (..),
    pmMetadata,
  )
where

import Data.Kind (Type)
import ExoMonad.Control.Role.Hook.Definitions (PMHooks)
import ExoMonad.Control.Role.Tool.Definitions (PMTools)
import ExoMonad.Control.Role.Types (RoleMetadata (..))
import GHC.Generics (Generic)

-- | Project Manager role record.
data PMRole mode = PMRole
  { pmToolsRecord :: PMTools mode,
    pmMetadata :: RoleMetadata,
    pmHooks :: PMHooks mode
  }
  deriving (Generic)

-- | PM role metadata.
pmMetadata :: RoleMetadata
pmMetadata =
  RoleMetadata
    { rmSlug = "pm",
      rmDisplayName = "Project Manager",
      rmDescription = "Planning agent: prioritizes work, monitors sprint health, approves expansions"
    }
