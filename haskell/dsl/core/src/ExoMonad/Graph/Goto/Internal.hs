-- | Internal module exposing OneOf and GotoChoice constructors.
--
-- __WARNING__: This module exposes constructors that should NOT be used
-- directly in application code. Pattern matching on 'OneOf' or 'GotoChoice'
-- bypasses the typed dispatch mechanism and creates fragile code.
--
-- = When to use this module
--
-- * 'Interpret.hs' - for implementing 'DispatchGoto' typeclass
-- * Test code - for assertions on specific positions
-- * Debugging - when you need to inspect the raw structure
--
-- = When NOT to use this module
--
-- * Application handlers - use 'gotoChoice', 'gotoExit', 'gotoSelf' instead
-- * Graph execution - use 'DispatchGoto' typeclass from 'Interpret.hs'
--
-- If you find yourself importing this module in application code, you're
-- probably fighting the type system. Ask: "Why do I need to pattern match?"
module ExoMonad.Graph.Goto.Internal
  ( -- * OneOf Constructors (for typed dispatch only)
    OneOf (..),

    -- * GotoChoice Constructor (for typed dispatch only)
    GotoChoice (..),

    -- * GotoAll Constructor (for parallel dispatch only)
    GotoAll (..),

    -- * Target Marker (re-exported for type families)
    To,

    -- * Type families (re-exported for convenience)
    Payloads,
    PayloadOf,
  )
where

import Data.Aeson (ToJSON (..), Value (..))
import Data.Kind (Type)
import Data.Vector qualified as V
import ExoMonad.Graph.Types (HList (..))

-- ════════════════════════════════════════════════════════════════════════════
-- ONEOF: TYPE-INDEXED SUM TYPE
-- ════════════════════════════════════════════════════════════════════════════

-- | A type-indexed sum type: "one of these types".
--
-- Position in the list encodes which type was chosen. Pattern matching is
-- fully typed - each case knows the exact payload type.
--
-- @
-- OneOf '[Int, String, Bool]
--   Here 42           -- an Int
--   There (Here "hi") -- a String
--   There (There (Here True)) -- a Bool
-- @
type OneOf :: [Type] -> Type
data OneOf ts where
  -- | The value is the first type in the list
  Here :: t -> OneOf (t ': ts)
  -- | The value is somewhere in the rest of the list
  There :: OneOf ts -> OneOf (t ': ts)

instance {-# OVERLAPPING #-} (ToJSON t) => ToJSON (OneOf '[t]) where
  toJSON (Here t) = toJSON t
  toJSON (There _) = error "Impossible: There in single-element OneOf"

instance {-# OVERLAPPABLE #-} (ToJSON t, ToJSON (OneOf ts)) => ToJSON (OneOf (t ': ts)) where
  toJSON (Here t) = toJSON t
  toJSON (There rest) = toJSON rest

-- ════════════════════════════════════════════════════════════════════════════
-- HLIST TOJSON INSTANCES
-- ════════════════════════════════════════════════════════════════════════════

-- | ToJSON instance for HList - converts to JSON array
instance ToJSON (HList '[]) where
  toJSON HNil = toJSON ([] :: [Value])

instance (ToJSON t, ToJSON (HList ts)) => ToJSON (HList (t ': ts)) where
  toJSON (t ::: ts) = case (toJSON t, toJSON ts) of
    (tVal, Array tsArray) -> Array (cons tVal tsArray)
    _ -> error "Impossible: HList ToJSON should produce Array"
    where
      cons x arr = case V.toList arr of
        xs -> V.fromList (x : xs)

-- ════════════════════════════════════════════════════════════════════════════
-- TARGET MARKER
-- ════════════════════════════════════════════════════════════════════════════

-- | Marker type for transition targets in 'GotoChoice'.
--
-- Unlike 'Goto' (which is an Effect), 'To' is a plain type-level marker
-- that represents a possible transition destination.
--
-- @
-- GotoChoice '[To "nodeA" PayloadA, To "nodeB" PayloadB, To Exit Response]
-- @
type To :: k -> Type -> Type
data To target payload

-- ════════════════════════════════════════════════════════════════════════════
-- GOTOCHOICE: HANDLER RETURN TYPE
-- ════════════════════════════════════════════════════════════════════════════

-- | Extract payload types from a list of 'To' markers.
--
-- @
-- Payloads '[To "a" Int, To "b" String, To Exit Bool] = '[Int, String, Bool]
-- @
type Payloads :: [Type] -> [Type]
type family Payloads targets where
  Payloads '[] = '[]
  Payloads (To name payload ': rest) = payload ': Payloads rest

-- | Extract the payload type from a 'To' marker.
type PayloadOf :: Type -> Type
type family PayloadOf t where
  PayloadOf (To name payload) = payload

-- | Return type for handlers that must choose a transition.
--
-- The @targets@ parameter is a type-level list of 'To' markers.
-- Internally wraps 'OneOf' for fully typed dispatch.
type GotoChoice :: [Type] -> Type
newtype GotoChoice targets = GotoChoice {unGotoChoice :: OneOf (Payloads targets)}

instance (ToJSON (OneOf (Payloads targets))) => ToJSON (GotoChoice targets) where
  toJSON (GotoChoice choice) = toJSON choice

-- ════════════════════════════════════════════════════════════════════════════
-- GOTOALL: PARALLEL FAN-OUT RETURN TYPE
-- ════════════════════════════════════════════════════════════════════════════

-- | Return type for handlers that fan-out to ALL targets in parallel.
--
-- Unlike 'GotoChoice' which picks ONE target (sum type), 'GotoAll' sends
-- to ALL targets simultaneously (product type).
--
-- The handler provides a payload for each target, and the runtime dispatches
-- to all of them concurrently.
--
-- @
-- -- Fan-out to two workers
-- fanoutHandler :: Task -> Eff es (key, GotoAll '[To "analyzer" Task, To "validator" Task])
-- fanoutHandler task = pure (task.orderId, gotoAll (analyzerTask ::: validatorTask ::: HNil))
-- @
--
-- Note: The handler also provides a correlation key that workers will use
-- to identify which fan-out batch their results belong to.
type GotoAll :: [Type] -> Type
newtype GotoAll targets = GotoAll {unGotoAll :: HList (Payloads targets)}

instance (ToJSON (HList (Payloads targets))) => ToJSON (GotoAll targets) where
  toJSON (GotoAll list) = toJSON list
