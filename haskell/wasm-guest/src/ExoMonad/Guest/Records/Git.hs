{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

-- | Git tool records.
module ExoMonad.Guest.Records.Git
  ( GitTools (..),
    gitToolsHandler,
    gitToolsSchema,
  )
where

import ExoMonad.Guest.Tool.Mode (AsHandler, AsSchema, ToolMode ((:-)), mkHandler, mkSchema)
import ExoMonad.Guest.Tools.Git (GitBranch, GitLog, GitStatus)
import GHC.Generics (Generic)
import Prelude hiding (log)

data GitTools mode = GitTools
  { branch :: mode :- GitBranch,
    status :: mode :- GitStatus,
    log :: mode :- GitLog
  }
  deriving (Generic)

gitToolsHandler :: GitTools AsHandler
gitToolsHandler =
  GitTools
    { branch = mkHandler @GitBranch,
      status = mkHandler @GitStatus,
      log = mkHandler @GitLog
    }

gitToolsSchema :: GitTools AsSchema
gitToolsSchema =
  GitTools
    { branch = mkSchema @GitBranch,
      status = mkSchema @GitStatus,
      log = mkSchema @GitLog
    }
