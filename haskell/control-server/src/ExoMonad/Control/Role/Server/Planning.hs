{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}

-- | Planning Server for the Role DSL.
--
-- Tools for project management and planning.
module ExoMonad.Control.Role.Server.Planning
  ( PlanningServer(..)
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import GHC.Generics (Generic)

import ExoMonad.Control.Role.Types (ToolField)
import ExoMonad.Control.Role.Tool.PMStatus (PMStatus)

-- | MCP Server for planning tools.
data PlanningServer mode (es :: [Type -> Type]) = PlanningServer
  { plDescription :: Text
  , plPMStatus :: ToolField mode es PMStatus
  }
  deriving Generic
