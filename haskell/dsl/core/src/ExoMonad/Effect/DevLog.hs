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
    GraphTransitionInfo (..)
  , StateSnapshotInfo (..)
  , LLMRequestInfo (..)
  , LLMResponseInfo (..)
  , ErrorContextInfo (..)

    -- * Verbosity (kept for backward compat)
  , Verbosity (..)
  ) where

import Data.Aeson (ToJSON (..))
import GHC.Generics (Generic)

-- Re-export event types from Log
import ExoMonad.Effect.Log
  ( GraphTransitionInfo(..)
  , StateSnapshotInfo(..)
  , LLMRequestInfo(..)
  , LLMResponseInfo(..)
  , ErrorContextInfo(..)
  )

-- | Verbosity levels for filtering log output.
--
-- DEPRECATED: Use LogLevel from ExoMonad.Effect.Log instead.
-- Mapping: VQuiet -> Error, VNormal -> Info, VVerbose -> Debug, VTrace -> Trace
data Verbosity
  = VQuiet    -- ^ Errors only
  | VNormal   -- ^ Transitions, state deltas, LLM summaries (default)
  | VVerbose  -- ^ Full LLM prompts/responses
  | VTrace    -- ^ Handler internal decisions
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON Verbosity
