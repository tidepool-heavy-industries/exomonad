{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}

-- | Developer Role Definition.
--
-- The Dev role executes implementation tasks: writing code, filing PRs, using LSP tools.
module ExoMonad.Control.Role.Definition.Dev
  ( DevRole(..)
  , devMetadata
  ) where

import Data.Kind (Type)
import GHC.Generics (Generic)

import ExoMonad.Control.Role.Types
  ( Hooks
  , RoleMetadata(..)
  , ServerField
  )
import ExoMonad.Control.Role.Server.Workflow (WorkflowServer)
import ExoMonad.Control.Role.Server.TUI (TUIServer)
import ExoMonad.Control.Role.Server.GitHub (GitHubServer)

-- | Developer role record.
data DevRole mode (es :: [Type -> Type]) = DevRole
  { devMetadataField :: RoleMetadata
  , devHooks         :: Hooks es
  , devWorkflow      :: ServerField mode es WorkflowServer
  , devTUI           :: ServerField mode es TUIServer
  , devGitHub        :: ServerField mode es GitHubServer
  }
  deriving Generic

-- | Dev role metadata.
devMetadata :: RoleMetadata
devMetadata = RoleMetadata
  { rmSlug = "dev"
  , rmDisplayName = "Developer"
  , rmDescription = "Implementation agent: writes code, files PRs, uses code intelligence tools"
  }
