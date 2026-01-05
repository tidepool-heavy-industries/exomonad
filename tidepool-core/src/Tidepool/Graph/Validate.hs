{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Compile-time validation for record-based Graph definitions.
--
-- This module provides utilities for error messages and validation.
-- The actual validation constraints for record-based graphs are in
-- Tidepool.Graph.Generic (ValidGraphRecord).
module Tidepool.Graph.Validate
  ( -- * Structural Validation Constraints (used by record DSL)
    AllNodesReachable
  , AllLogicNodesReachExit
  , NoDeadGotos

    -- * Error Messages (reusable)
  , UnsatisfiedNeedError
  , UnsatisfiedNeedErrorWithContext
  , InvalidGotoTargetError
  , InvalidGotoTargetErrorWithContext
  , DuplicateSchemaError
  , DuplicateSchemaErrorWithNodes

    -- * Error Formatting Helpers
  , FormatTypeList
  , FormatSymbolList

    -- * Structural Error Messages
  , UnreachableNodeError
  , NoExitPathError
  , DeadGotoError

    -- * Backend Compatibility Errors
  , ClaudeCodeCFBackendError
  ) where

import Data.Kind (Type, Constraint)
import GHC.TypeLits (Symbol, TypeError, ErrorMessage(..))

-- Re-export record-based structural validation (with type family wrappers for compatibility)
import Tidepool.Graph.Validate.RecordStructure
  ( AllFieldsReachable
  , AllLogicFieldsReachExit
  , NoDeadGotosRecord
  , UnreachableFieldError
  , NoExitPathFieldError
  , DeadGotoFieldError
  , FormatTypeList
  )

-- Create type family wrappers for backward compatibility
-- These forward to the record-based validation constraints
type AllNodesReachable :: (Type -> Type) -> Constraint
type family AllNodesReachable graph where
  AllNodesReachable graph = AllFieldsReachable graph

type AllLogicNodesReachExit :: (Type -> Type) -> Constraint
type family AllLogicNodesReachExit graph where
  AllLogicNodesReachExit graph = AllLogicFieldsReachExit graph

type NoDeadGotos :: (Type -> Type) -> Constraint
type family NoDeadGotos graph where
  NoDeadGotos graph = NoDeadGotosRecord graph

type UnreachableNodeError :: Symbol -> Constraint
type family UnreachableNodeError name where
  UnreachableNodeError name = UnreachableFieldError name

type NoExitPathError :: Symbol -> Constraint
type family NoExitPathError name where
  NoExitPathError name = NoExitPathFieldError name

type DeadGotoError :: Symbol -> Symbol -> Type -> Constraint
type family DeadGotoError srcName targetName payload where
  DeadGotoError srcName targetName payload = DeadGotoFieldError srcName targetName payload

-- ════════════════════════════════════════════════════════════════════════════
-- ERROR MESSAGES
-- ════════════════════════════════════════════════════════════════════════════

-- | Error when a node's Need is not provided by any Schema or Entry.
--
-- This version takes the list of provided types so it can show what IS available.
type UnsatisfiedNeedError :: Symbol -> Type -> Constraint
type UnsatisfiedNeedError nodeName needType = TypeError
  ('Text "Graph validation failed: unsatisfied dependency"
   ':$$: 'Text ""
   ':$$: 'Text "Node '" ':<>: 'Text nodeName ':<>: 'Text "' needs type:"
   ':$$: 'Text "  " ':<>: 'ShowType needType
   ':$$: 'Text ""
   ':$$: 'Text "But no node provides this type via Schema and Entry doesn't provide it."
   ':$$: 'Text ""
   ':$$: 'Text "Fix options:"
   ':$$: 'Text "  1. Add a node with 'Schema " ':<>: 'ShowType needType ':<>: 'Text "'"
   ':$$: 'Text "  2. Change Entry to provide this type: Entry :~> " ':<>: 'ShowType needType
   ':$$: 'Text "  3. Remove " ':<>: 'ShowType needType ':<>: 'Text " from this node's Needs"
  )

-- | Enhanced error showing what types ARE available.
type UnsatisfiedNeedErrorWithContext :: Symbol -> Type -> [Type] -> Constraint
type UnsatisfiedNeedErrorWithContext nodeName needType available = TypeError
  ('Text "Graph validation failed: unsatisfied dependency"
   ':$$: 'Text ""
   ':$$: 'Text "Node '" ':<>: 'Text nodeName ':<>: 'Text "' needs type:"
   ':$$: 'Text "  " ':<>: 'ShowType needType
   ':$$: 'Text ""
   ':$$: 'Text "Available types (from Entry and Schema outputs):"
   ':$$: FormatTypeList available
   ':$$: 'Text ""
   ':$$: 'Text "Fix options:"
   ':$$: 'Text "  1. Add a node with 'Schema " ':<>: 'ShowType needType ':<>: 'Text "'"
   ':$$: 'Text "  2. Change Entry to provide this type"
   ':$$: 'Text "  3. Remove " ':<>: 'ShowType needType ':<>: 'Text " from this node's Needs"
  )

-- | Error when a Goto target doesn't exist.
type InvalidGotoTargetError :: Symbol -> Symbol -> Constraint
type InvalidGotoTargetError srcName targetName = TypeError
  ('Text "Graph validation failed: invalid Goto target"
   ':$$: 'Text ""
   ':$$: 'Text "Node '" ':<>: 'Text srcName ':<>: 'Text "' has:"
   ':$$: 'Text "  Goto \"" ':<>: 'Text targetName ':<>: 'Text "\" ..."
   ':$$: 'Text ""
   ':$$: 'Text "But no node named \"" ':<>: 'Text targetName ':<>: 'Text "\" exists."
   ':$$: 'Text ""
   ':$$: 'Text "Fix options:"
   ':$$: 'Text "  1. Create a node named \"" ':<>: 'Text targetName ':<>: 'Text "\""
   ':$$: 'Text "  2. Use 'Goto Exit' for graph termination"
   ':$$: 'Text "  3. Check spelling of the target node name"
  )

-- | Enhanced error showing available node names.
type InvalidGotoTargetErrorWithContext :: Symbol -> Symbol -> [Symbol] -> Constraint
type InvalidGotoTargetErrorWithContext srcName targetName validNames = TypeError
  ('Text "Graph validation failed: invalid Goto target"
   ':$$: 'Text ""
   ':$$: 'Text "Node '" ':<>: 'Text srcName ':<>: 'Text "' has:"
   ':$$: 'Text "  Goto \"" ':<>: 'Text targetName ':<>: 'Text "\" ..."
   ':$$: 'Text ""
   ':$$: 'Text "But no node named \"" ':<>: 'Text targetName ':<>: 'Text "\" exists."
   ':$$: 'Text ""
   ':$$: 'Text "Available node names:"
   ':$$: FormatSymbolList validNames
   ':$$: 'Text ""
   ':$$: 'Text "Fix options:"
   ':$$: 'Text "  1. Create a node named \"" ':<>: 'Text targetName ':<>: 'Text "\""
   ':$$: 'Text "  2. Use 'Goto Exit' for graph termination"
   ':$$: 'Text "  3. Use one of the existing node names above"
  )

-- | Format a list of symbols for display in error messages.
type FormatSymbolList :: [Symbol] -> ErrorMessage
type family FormatSymbolList ss where
  FormatSymbolList '[] = 'Text "  (none)"
  FormatSymbolList '[s] = 'Text "  • " ':<>: 'Text s
  FormatSymbolList (s ': rest) = 'Text "  • " ':<>: 'Text s ':$$: FormatSymbolList rest


-- | Error when two nodes have the same Schema type, showing which nodes conflict.
type DuplicateSchemaErrorWithNodes :: Symbol -> Symbol -> Type -> Constraint
type DuplicateSchemaErrorWithNodes node1 node2 schemaType = TypeError
  ('Text "Graph validation failed: duplicate Schema type"
   ':$$: 'Text ""
   ':$$: 'Text "Multiple nodes produce the same Schema type:"
   ':$$: 'Text "  • Node '" ':<>: 'Text node1 ':<>: 'Text "' has Schema " ':<>: 'ShowType schemaType
   ':$$: 'Text "  • Node '" ':<>: 'Text node2 ':<>: 'Text "' has Schema " ':<>: 'ShowType schemaType
   ':$$: 'Text ""
   ':$$: 'Text "This creates ambiguous Needs resolution - which node provides the type?"
   ':$$: 'Text ""
   ':$$: 'Text "Fix options:"
   ':$$: 'Text "  1. Use distinct wrapper types: data " ':<>: 'Text node1 ':<>: 'Text "Response = ..."
   ':$$: 'Text "  2. Merge the nodes if they serve the same purpose"
   ':$$: 'Text "  3. Use different Schema types for different semantics"
  )

-- | Simple duplicate error (kept for backwards compatibility)
type DuplicateSchemaError :: Type -> Constraint
type DuplicateSchemaError t = TypeError
  ('Text "Graph validation failed: duplicate Schema type"
   ':$$: 'Text "  Schema " ':<>: 'ShowType t ':<>: 'Text " is produced by multiple nodes."
   ':$$: 'Text "This creates ambiguous Needs resolution."
   ':$$: 'Text "Fix: Use distinct types for each node's Schema output."
  )

-- ════════════════════════════════════════════════════════════════════════════
-- BACKEND COMPATIBILITY ERRORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Error when ClaudeCode annotation is used with CloudflareAI backend.
--
-- ClaudeCode spawns a local subprocess via zellij-cc, which is not available
-- in Cloudflare Workers. This error triggers at compile time when a graph
-- has both ':& Backend CloudflareAI' and any node with ':@ ClaudeCode'.
--
-- @
-- -- This will produce the error:
-- type BadGraph = Graph '[...]
--   :@ ClaudeCode 'Sonnet 'Nothing
--   :& Backend CloudflareAI
-- @
type ClaudeCodeCFBackendError :: Symbol -> Constraint
type ClaudeCodeCFBackendError nodeName = TypeError
  ('Text "Graph validation failed: incompatible backend for ClaudeCode"
   ':$$: 'Text ""
   ':$$: 'Text "Node '" ':<>: 'Text nodeName ':<>: 'Text "' has ClaudeCode annotation,"
   ':$$: 'Text "but this graph uses CloudflareAI backend."
   ':$$: 'Text ""
   ':$$: 'Text "ClaudeCode spawns a local subprocess via zellij-cc,"
   ':$$: 'Text "which is not available in Cloudflare Workers."
   ':$$: 'Text ""
   ':$$: 'Text "Fix options:"
   ':$$: 'Text "  1. Use 'Backend NativeAnthropic' for this graph"
   ':$$: 'Text "  2. Remove the ClaudeCode annotation from node '" ':<>: 'Text nodeName ':<>: 'Text "'"
   ':$$: 'Text "  3. Move ClaudeCode nodes to a separate NativeAnthropic graph"
  )
