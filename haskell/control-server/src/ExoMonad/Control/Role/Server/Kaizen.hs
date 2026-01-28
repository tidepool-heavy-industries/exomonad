{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}

module ExoMonad.Control.Role.Server.Kaizen
  ( KaizenServer(..)
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import GHC.Generics (Generic)

import ExoMonad.Control.Role.Types (ToolField)
import ExoMonad.Control.Role.Tool.KaizenReport (KaizenReport)

-- | Server for Kaizen (Continuous Improvement) tools.
data KaizenServer mode (es :: [Type -> Type]) = KaizenServer
  { kzDescription :: Text
  , kzReport      :: ToolField mode es KaizenReport
  }
  deriving Generic
