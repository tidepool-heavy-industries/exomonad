{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | Structural validation for list-based Graph topology.
--
-- This module is kept for backwards compatibility but is no longer used.
-- The record-based DSL uses Tidepool.Graph.Validate.RecordStructure instead.
--
-- For new code, use:
-- * AllFieldsReachable (instead of AllNodesReachable)
-- * AllLogicFieldsReachExit (instead of AllLogicNodesReachExit)
-- * NoDeadGotosRecord (instead of NoDeadGotos)
module Tidepool.Graph.Validate.Structure
  ( -- * Deprecated (use RecordStructure instead)
  ) where

-- This module is kept for backwards compatibility but exports nothing.
-- All structural validation is now in RecordStructure for record-based graphs.
