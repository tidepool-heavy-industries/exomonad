{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Graph registry and FFI typeclass definitions.
--
-- This module provides the infrastructure for registering graphs that can be
-- exposed via FFI. The actual 'GraphRegistry' type and 'GraphFFI' instances
-- are defined in exomonad-wasm to avoid circular dependencies.
--
-- = Usage
--
-- In exomonad-wasm, define your registry and instances:
--
-- @
-- type GraphRegistry = '[TestGraph, ExampleGraph, HabiticaRoutingGraph]
--
-- instance GraphMeta TestGraph where
--   graphId _ = "test"
--   type GraphEntry TestGraph = Int
--   type GraphExit TestGraph = Int  -- Unwrapped from GotoChoice
--
-- instance GraphRunner TestGraph where
--   runGraph = computeHandlerWasm
--   graphStateRef _ = testGraphState
-- @
--
-- Then in Ffi.hs, use the TH splice:
--
-- @
-- $(deriveAllFFIExports ''GraphRegistry)
-- @
module ExoMonad.Generated.Registry
  ( -- * Metadata Typeclass
    GraphMeta(..)

    -- * Type-Level Registry Helpers
  , AllGraphMeta
  , GraphIds
  , ReifyGraphIds(..)

    -- * Effect Detection
  , GraphEffects(..)
  ) where

import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import GHC.TypeLits (Symbol)


-- ════════════════════════════════════════════════════════════════════════════
-- METADATA TYPECLASS
-- ════════════════════════════════════════════════════════════════════════════

-- | Metadata for a graph that can be exposed via FFI.
--
-- This typeclass provides compile-time information about a graph's
-- entry/exit types and runtime identifier.
--
-- The actual runner (execution function) and state ref are defined
-- separately in exomonad-wasm to avoid circular dependencies.
class GraphMeta (g :: Type -> Type) where
  -- | Unique identifier for this graph, used in FFI function names.
  --
  -- Example: @"test"@ generates @initialize_test@, @step_test@, etc.
  graphId :: Proxy g -> Text

  -- | The entry type for this graph (what the graph accepts as input).
  type GraphEntry g :: Type

  -- | The exit type for this graph (unwrapped from GotoChoice).
  --
  -- This should be the payload type, not @GotoChoice '[To Exit T]@.
  type GraphExit g :: Type


-- ════════════════════════════════════════════════════════════════════════════
-- TYPE-LEVEL REGISTRY HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Constraint that all graphs in a list have 'GraphMeta' instances.
type AllGraphMeta :: [Type -> Type] -> Constraint
type family AllGraphMeta gs where
  AllGraphMeta '[] = ()
  AllGraphMeta (g ': gs) = (GraphMeta g, AllGraphMeta gs)

-- | Extract graph IDs from a registry at the type level.
--
-- This is useful for generating TypeScript types and WASM exports.
type GraphIds :: [Type -> Type] -> [Symbol]
type family GraphIds gs where
  GraphIds '[] = '[]
  GraphIds (g ': gs) = GraphIdOf g ': GraphIds gs

-- | Get the graph ID as a type-level Symbol.
--
-- This requires a type family since we can't use the value-level 'graphId'
-- at the type level directly.
type family GraphIdOf (g :: Type -> Type) :: Symbol

-- | Reify graph IDs from a registry to runtime.
class ReifyGraphIds (gs :: [Type -> Type]) where
  reifyGraphIds :: Proxy gs -> [Text]

instance ReifyGraphIds '[] where
  reifyGraphIds _ = []

instance (GraphMeta g, ReifyGraphIds gs) => ReifyGraphIds (g ': gs) where
  reifyGraphIds _ = graphId (Proxy @g) : reifyGraphIds (Proxy @gs)


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT DETECTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Effects required by a graph.
--
-- This is used to generate the TypeScript effects map, which tells
-- the TypeScript runtime which effect handlers are needed.
class GraphEffects (g :: Type -> Type) where
  -- | List of effect type names this graph uses.
  --
  -- Example: @["Log", "LlmComplete", "Habitica"]@
  graphEffects :: Proxy g -> [Text]

  -- Default: empty list (no special effects beyond Log)
  graphEffects _ = []
