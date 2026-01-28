{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Control.ExoTools.Status
  ( exoStatusLogic
  , ExoStatusArgs(..)
  , ExoStatusResult(..)
  ) where

import Control.Monad.Freer (Eff, Member)

import ExoMonad.Effects.Git (Git)
import ExoMonad.Effects.GitHub (GitHub)

import ExoMonad.Control.ExoTools.Internal (ExoStatusResult(..), getDevelopmentContext)
import ExoMonad.Control.ExoTools.Status.Types

-- | Core logic for exo_status.
exoStatusLogic
  :: (Member Git es, Member GitHub es)
  => ExoStatusArgs
  -> Eff es ExoStatusResult
exoStatusLogic _args = getDevelopmentContext Nothing
