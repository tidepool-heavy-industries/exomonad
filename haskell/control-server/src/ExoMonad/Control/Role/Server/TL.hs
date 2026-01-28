{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}

-- | Team Lead Server for the Role DSL.
module ExoMonad.Control.Role.Server.TL
  ( TLServer(..)
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import GHC.Generics (Generic)

import ExoMonad.Control.Role.Types (ToolField)
import ExoMonad.Control.Role.Tool.TLCreateIssue (TLCreateIssue)

-- | MCP Server for Team Lead tools.
data TLServer mode (es :: [Type -> Type]) = TLServer
  { tlCreateIssue :: ToolField mode es TLCreateIssue
  }
  deriving Generic
