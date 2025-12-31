{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Typed graph executor using OneOf dispatch.
--
-- This module provides fully typed graph execution without Dynamic or
-- unsafeCoerce. The key insight is that 'GotoChoice' wraps 'OneOf', where
-- position encodes which target was chosen and pattern matching gives exact
-- typed payloads.
--
-- = Design
--
-- @
-- GotoChoice '[To \"nodeA\" PayloadA, To \"nodeB\" PayloadB, To Exit Result]
--   = GotoChoice (OneOf '[PayloadA, PayloadB, Result])
--
-- Pattern matching:
--   Here payloadA      → call nodeA handler with PayloadA
--   There (Here payloadB) → call nodeB handler with PayloadB
--   There (There (Here result)) → return Result (exit)
-- @
--
-- The 'DispatchGoto' typeclass inductively pattern matches on the target list,
-- using 'HasField' to get handlers from the graph record by name.
module Tidepool.Graph.Execute
  ( -- * Dispatch Typeclass
    DispatchGoto(..)
    -- * Graph Execution
  , runGraph
  , runGraphFrom
    -- * Entry Handler Discovery
  , FindEntryHandler
  ) where

import Data.Kind (Constraint, Type)
import Effectful (Effect, Eff)
import GHC.Generics (Generic(..))
import GHC.Records (HasField(..))
import GHC.TypeLits (Symbol, KnownSymbol, TypeError, ErrorMessage(..))

import Tidepool.Graph.Edges (GetNeeds)
import Tidepool.Graph.Generic (AsHandler, FieldsWithNamesOf)
import Tidepool.Graph.Generic.Core (Entry, AsGraph)
import qualified Tidepool.Graph.Generic.Core as G (Exit)
import Tidepool.Graph.Goto (GotoChoice(..), OneOf(..), To)
import Tidepool.Graph.Types (Exit, Self)


-- ════════════════════════════════════════════════════════════════════════════
-- TYPE-LEVEL UTILITIES (local definitions)
-- ════════════════════════════════════════════════════════════════════════════

-- | Type-level If (polykinded).
type IfMaybe :: Bool -> Maybe k -> Maybe k -> Maybe k
type family IfMaybe cond t f where
  IfMaybe 'True  t _ = t
  IfMaybe 'False _ f = f

-- | Check if an element is in a type-level list (polykinded).
type ElemType :: k -> [k] -> Bool
type family ElemType x xs where
  ElemType _ '[] = 'False
  ElemType x (x ': _) = 'True
  ElemType x (_ ': rest) = ElemType x rest


-- ════════════════════════════════════════════════════════════════════════════
-- ENTRY HANDLER DISCOVERY
-- ════════════════════════════════════════════════════════════════════════════

-- | Find the first field whose node definition accepts the entry type.
--
-- Iterates through (fieldName, nodeDef) pairs from 'FieldsWithNamesOf' and
-- returns the first field name where 'GetNeeds' contains the entry type.
--
-- @
-- -- For a graph with:
-- --   entry   :: mode :- Entry Int
-- --   compute :: mode :- LogicNode :@ Needs '[Int] :@ UsesEffects '[...]
-- --   exit    :: mode :- Exit Int
-- --
-- FindEntryHandler Int fields = 'Just "compute"
-- @
type FindEntryHandler :: Type -> [(Symbol, Type)] -> Maybe Symbol
type family FindEntryHandler entryType fields where
  FindEntryHandler _ '[] = 'Nothing
  FindEntryHandler entryType ('(name, Entry _) ': rest) =
    FindEntryHandler entryType rest  -- Skip Entry marker
  FindEntryHandler entryType ('(name, G.Exit _) ': rest) =
    FindEntryHandler entryType rest  -- Skip Exit marker
  FindEntryHandler entryType ('(name, def) ': rest) =
    IfMaybe (ElemType entryType (GetNeeds def))
            ('Just name)
            (FindEntryHandler entryType rest)


-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH EXECUTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Run a graph starting from a named handler.
--
-- Use this when you want to explicitly specify which handler to start from,
-- bypassing automatic entry handler discovery.
--
-- @
-- result <- runGraphFrom @"compute" handlers inputValue
-- @
runGraphFrom
  :: forall (name :: Symbol) graph entryType targets exitType es.
     ( KnownSymbol name
     , HasField name (graph (AsHandler es)) (entryType -> Eff es (GotoChoice targets))
     , DispatchGoto graph targets es exitType
     )
  => graph (AsHandler es)
  -> entryType
  -> Eff es exitType
runGraphFrom graph input = do
  let handler = getField @name graph
  choice <- handler input
  dispatchGoto graph choice


-- | Run a graph from Entry to Exit.
--
-- Automatically discovers the first handler that accepts the entry type
-- (via the 'Needs' annotation), calls it with the input, and dispatches
-- through the graph until Exit is reached.
--
-- @
-- -- Define graph
-- data TestGraph mode = TestGraph
--   { entry   :: mode :- Entry Int
--   , compute :: mode :- LogicNode :@ Needs '[Int] :@ UsesEffects '[Goto Exit Int]
--   , exit    :: mode :- Exit Int
--   }
--
-- -- Run it
-- result <- runGraph handlers 5  -- Returns 6 (if compute does +1)
-- @
runGraph
  :: forall graph entryType targets exitType es entryHandlerName.
     ( Generic (graph AsGraph)
     , FindEntryHandler entryType (FieldsWithNamesOf graph) ~ 'Just entryHandlerName
     , KnownSymbol entryHandlerName
     , HasField entryHandlerName (graph (AsHandler es)) (entryType -> Eff es (GotoChoice targets))
     , DispatchGoto graph targets es exitType
     )
  => graph (AsHandler es)
  -> entryType
  -> Eff es exitType
runGraph = runGraphFrom @entryHandlerName


-- ════════════════════════════════════════════════════════════════════════════
-- DISPATCH TYPECLASS
-- ════════════════════════════════════════════════════════════════════════════

-- | Dispatch on a 'GotoChoice', recursively executing handlers until Exit.
--
-- This typeclass enables fully typed dispatch through a graph. Each instance
-- handles one case of the target list, recursively calling handlers until
-- an Exit target is reached.
--
-- = Type Parameters
--
-- * @graph@ - The graph record type (e.g., @TestGraph@)
-- * @targets@ - The current target list to dispatch on
-- * @es@ - The effect stack available to handlers
-- * @exitType@ - The graph's exit type (what 'runGraph' returns)
--
-- = How It Works
--
-- Given @GotoChoice '[To \"a\" A, To \"b\" B, To Exit R]@:
--
-- 1. Pattern match on @OneOf '[A, B, R]@
-- 2. If @Here payload@: call handler \"a\" with payload :: A
-- 3. If @There (Here payload)@: call handler \"b\" with payload :: B
-- 4. If @There (There (Here result))@: return result :: R
--
-- Each handler returns its own @GotoChoice@, so dispatch recurses through
-- the graph until Exit is reached.
--
-- = Example
--
-- @
-- -- Given a graph with compute handler that exits
-- result <- dispatchGoto testHandlers (gotoChoice \@"compute" 5)
-- -- Internally: calls compute handler, gets GotoChoice '[To Exit Int]
-- -- Pattern matches Here 6, returns 6
-- @
type DispatchGoto :: (Type -> Type) -> [Type] -> [Effect] -> Type -> Constraint
class DispatchGoto graph targets es exitType where
  dispatchGoto :: graph (AsHandler es) -> GotoChoice targets -> Eff es exitType


-- ════════════════════════════════════════════════════════════════════════════
-- ERROR CASE: EMPTY TARGET LIST
-- ════════════════════════════════════════════════════════════════════════════

-- | Error case: Empty target list is invalid.
--
-- A GotoChoice must have at least one target. This instance produces a clear
-- type error rather than an opaque "No instance" message.
instance TypeError
  ('Text "Cannot dispatch on empty target list"
   ':$$: 'Text ""
   ':$$: 'Text "A GotoChoice must have at least one target (typically To Exit exitType)."
   ':$$: 'Text "Check that your UsesEffects annotation includes valid Goto effects."
  ) => DispatchGoto graph '[] es exitType where
  dispatchGoto = error "unreachable: empty target list"


-- ════════════════════════════════════════════════════════════════════════════
-- EXIT INSTANCES
-- ════════════════════════════════════════════════════════════════════════════

-- | Base case: Exit is the only target.
--
-- When there's only @To Exit exitType@ in the target list, pattern matching
-- gives us the exit value directly.
instance {-# OVERLAPPING #-} DispatchGoto graph '[To Exit exitType] es exitType where
  dispatchGoto _ (GotoChoice (Here result)) = pure result

-- | Exit is first, but there are more targets.
--
-- Handle the Exit case (return result) or skip to rest of targets.
instance {-# OVERLAPPABLE #-}
  ( DispatchGoto graph rest es exitType
  ) => DispatchGoto graph (To Exit exitType ': rest) es exitType where
  dispatchGoto _ (GotoChoice (Here result)) = pure result
  dispatchGoto graph (GotoChoice (There rest)) =
    dispatchGoto @graph @rest graph (GotoChoice rest)


-- ════════════════════════════════════════════════════════════════════════════
-- SELF-LOOP INSTANCES
-- ════════════════════════════════════════════════════════════════════════════

-- | Self-loop as only target: not yet implemented.
--
-- Self-loops require tracking the "current" handler to re-invoke.
-- This instance provides a clear error rather than failing with HasField.
instance TypeError
  ('Text "Self-loop dispatch not yet implemented"
   ':$$: 'Text ""
   ':$$: 'Text "To Self transitions require a graph runner that tracks"
   ':$$: 'Text "the current node. Use explicit Goto to a named node instead."
  ) => DispatchGoto graph '[To Self payload] es exitType where
  dispatchGoto = error "unreachable: self-loop"

-- | Self first with more targets: not yet implemented.
instance {-# OVERLAPPABLE #-} TypeError
  ('Text "Self-loop dispatch not yet implemented"
   ':$$: 'Text ""
   ':$$: 'Text "To Self transitions require a graph runner that tracks"
   ':$$: 'Text "the current node. Use explicit Goto to a named node instead."
  ) => DispatchGoto graph (To Self payload ': rest) es exitType where
  dispatchGoto = error "unreachable: self-loop"


-- ════════════════════════════════════════════════════════════════════════════
-- NAMED NODE INSTANCES
-- ════════════════════════════════════════════════════════════════════════════

-- | Named node target: call the handler and recurse.
--
-- When the first target is @To (name :: Symbol) payload@:
--
-- 1. Use 'HasField' to get the handler from the graph record
-- 2. Call handler with the payload (fully typed!)
-- 3. Recursively dispatch on the handler's returned 'GotoChoice'
--
-- The @handlerTargets@ type variable is inferred from the handler's return
-- type, ensuring type safety throughout the dispatch chain.
instance
  ( KnownSymbol name
  , HasField name (graph (AsHandler es)) (payload -> Eff es (GotoChoice handlerTargets))
  , DispatchGoto graph handlerTargets es exitType
  , DispatchGoto graph rest es exitType
  ) => DispatchGoto graph (To (name :: Symbol) payload ': rest) es exitType where

  dispatchGoto graph (GotoChoice (Here payload)) = do
    let handler = getField @name graph
    nextChoice <- handler payload
    dispatchGoto graph nextChoice

  dispatchGoto graph (GotoChoice (There rest)) =
    dispatchGoto @graph @rest graph (GotoChoice rest)
