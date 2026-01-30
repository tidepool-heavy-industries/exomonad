{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}

-- | Developer Role Definition.
--
-- The Dev role executes implementation tasks: writing code, filing PRs, using LSP tools.
module ExoMonad.Control.Role.Definition.Dev
  ( DevRole (..),
    devMetadata,
  )
where

import Data.Kind (Type)
import ExoMonad.Control.Role.Hook.Definitions (DevHooks)
import ExoMonad.Control.Role.Tool.Definitions (DevTools)
import ExoMonad.Control.Role.Types (RoleMetadata (..))
import GHC.Generics (Generic)

-- | Developer role record.
data DevRole mode = DevRole
  { devToolsRecord :: DevTools mode,
    devMetadata :: RoleMetadata,
    devHooks :: DevHooks mode
  }
  deriving (Generic)

-- | Dev role metadata.
devMetadata :: RoleMetadata
devMetadata =
  RoleMetadata
    { rmSlug = "dev",
      rmDisplayName = "Developer",
      rmDescription = "Implementation agent: writes code, files PRs, uses code intelligence tools"
    }
