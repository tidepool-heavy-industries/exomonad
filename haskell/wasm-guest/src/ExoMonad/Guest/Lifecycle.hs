-- | Agent lifecycle phase types and KV-backed persistence.
--
-- Re-exports from submodules for backward compatibility and convenience.
-- Transition modules with conflicting names (canExit) are NOT re-exported
-- and must be imported qualified.
module ExoMonad.Guest.Lifecycle
  ( -- * Dev state
    module ExoMonad.Guest.Lifecycle.DevState,

    -- * TL state
    module ExoMonad.Guest.Lifecycle.TLState,

    -- * Worker state
    module ExoMonad.Guest.Lifecycle.WorkerState,

    -- * Phase persistence
    module ExoMonad.Guest.Lifecycle.PhaseEffect,
  )
where

import ExoMonad.Guest.Lifecycle.DevState
import ExoMonad.Guest.Lifecycle.PhaseEffect
import ExoMonad.Guest.Lifecycle.TLState
import ExoMonad.Guest.Lifecycle.WorkerState
