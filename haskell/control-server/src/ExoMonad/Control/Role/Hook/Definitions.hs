{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}

module ExoMonad.Control.Role.Hook.Definitions
  ( CommonHooks(..)
  , TLHooks(..)
  , DevHooks(..)
  , PMHooks(..)
  ) where

import GHC.Generics (Generic)
import ExoMonad.Graph.Generic ((:-), Hook)
import ExoMonad.Control.Role.Types
  ( SessionStartInput, SessionStartResponse
  , PreToolUseInput, PreToolUseResponse
  , PostToolUseInput
  , StopInput, StopResponse
  , Notification
  , SessionEndInput
  )

-- | Common hooks shared by all roles
data CommonHooks mode = CommonHooks
  { sessionStart :: mode :- Hook SessionStartInput SessionStartResponse
  , preToolUse   :: mode :- Hook PreToolUseInput PreToolUseResponse
  , postToolUse  :: mode :- Hook PostToolUseInput ()
  , stop         :: mode :- Hook StopInput StopResponse
  , sessionEnd   :: mode :- Hook SessionEndInput ()
  , notification :: mode :- Hook Notification ()
  , subagentStop :: mode :- Hook SessionEndInput () 
  } deriving Generic

-- Role-specific hook records
data TLHooks mode = TLHooks
  { common :: CommonHooks mode
  } deriving Generic

data DevHooks mode = DevHooks
  { common :: CommonHooks mode
  } deriving Generic

data PMHooks mode = PMHooks
  { common :: CommonHooks mode
  } deriving Generic
