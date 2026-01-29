{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Developer Role Definition.
--
-- The Dev role executes implementation tasks: writing code, filing PRs, using LSP tools.
module ExoMonad.Control.Role.Definition.Dev
  ( DevRole(..)
  , devMetadata
  ) where

import Data.Kind (Type)
import GHC.Generics (Generic)

import ExoMonad.Control.Role.Types (Hooks, RoleMetadata(..))
import ExoMonad.Control.Role.Tool.Definitions (DevTools)

-- | Developer role record.
data DevRole mode (es :: [Type -> Type]) = DevRole
  { devToolsRecord :: DevTools mode
  , devMetadata    :: RoleMetadata
  , devHooks       :: Hooks es
  }
  deriving Generic

-- | Dev role metadata.
devMetadata :: RoleMetadata
devMetadata = RoleMetadata
  { rmSlug = "dev"
  , rmDisplayName = "Developer"
  , rmDescription = "Implementation agent: writes code, files PRs, uses code intelligence tools"
  }
