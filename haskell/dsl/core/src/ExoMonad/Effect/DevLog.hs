{-# LANGUAGE StrictData #-}

-- | Legacy DevLog types - DEPRECATED.
--
-- The DevLog effect has been consolidated into 'ExoMonad.Effect.Log'.
-- This module re-exports the event types for backward compatibility.
--
-- == Migration Guide
--
-- Before:
-- @
-- import ExoMonad.Effect.DevLog (devLogGraph, GraphTransitionInfo(..))
-- devLogGraph GraphTransitionInfo { ... }
-- @
--
-- After:
-- @
-- import ExoMonad.Effect.Log (logGraph, GraphTransitionInfo(..))
-- logGraph GraphTransitionInfo { ... }
-- @
--
-- The event types are now defined in 'ExoMonad.Effect.Log'.
module ExoMonad.Effect.DevLog
  ( -- * Event Types (re-exported from Log)
    GraphTransitionInfo (..),
    StateSnapshotInfo (..),
    LLMRequestInfo (..),
    LLMResponseInfo (..),
    ErrorContextInfo (..),

    -- * Verbosity (kept for backward compat)
    Verbosity (..),
  )
where

import Data.Aeson (ToJSON (..))
-- Re-export event types from Log
import ExoMonad.Effect.Log
  ( ErrorContextInfo (..),
    GraphTransitionInfo (..),
    LLMRequestInfo (..),
    LLMResponseInfo (..),
    StateSnapshotInfo (..),
  )
import GHC.Generics (Generic)

-- | Verbosity levels for filtering log output.
--
-- DEPRECATED: Use LogLevel from ExoMonad.Effect.Log instead.
-- Mapping: VQuiet -> Error, VNormal -> Info, VVerbose -> Debug, VTrace -> Trace
data Verbosity
  = -- | Errors only
    VQuiet
  | -- | Transitions, state deltas, LLM summaries (default)
    VNormal
  | -- | Full LLM prompts/responses
    VVerbose
  | -- | Handler internal decisions
    VTrace
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON Verbosity
