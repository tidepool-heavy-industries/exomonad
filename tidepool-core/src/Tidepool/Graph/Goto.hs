{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The Goto effect for graph transitions.
--
-- Goto is the fundamental effect for Logic nodes to express state machine
-- transitions. Each Goto effect represents a possible transition to a
-- named target with a typed payload.
--
-- = Design Philosophy
--
-- "Everything is an effect. Transitions are the Goto effect."
--
-- Unlike traditional state machines with explicit edge declarations,
-- edges in the Graph DSL are derived from:
--
-- * 'Goto' effects in Logic node effect stacks (explicit transitions)
-- * 'Schema' outputs that flow to 'Needs' declarations (implicit data flow)
--
-- = Example
--
-- @
-- "router" := Logic
--     :@ Needs '[Message, Intent]
--     :@ Eff '[
--         Goto "refund" Message      -- Can transition to refund node
--       , Goto "support" Message     -- Or to support node
--       , Goto Exit Response         -- Or exit the graph
--       ]
-- @
--
-- The handler can then use:
--
-- @
-- routerHandler :: Message -> Intent -> Eff '[Goto "refund" Message, ...] ()
-- routerHandler msg intent = case intent of
--   Refund  -> goto @"refund" msg
--   Support -> goto @"support" msg
--   Done    -> goto @Exit (Response "Complete")
-- @
module Tidepool.Graph.Goto
  ( -- * The Goto Effect
    Goto(..)
  , goto

    -- * OneOf Sum Type
  , OneOf(..)
  , NonEmptyList
  , Payloads
  , PayloadOf
  , InjectTarget(..)
  -- Note: Inject has been removed from exports. It uses type equality which
  -- finds the first match in duplicate-type lists. Use InjectTarget instead,
  -- which matches on full (To name payload) markers for correct positioning.

    -- * GotoChoice Return Type
  , To
  , GotoChoice(..)
  , gotoChoice
  , gotoExit
  , gotoSelf

    -- * LLM Handler Variants
  , LLMHandler(..)

    -- * Target Validation
  , GotoElem
  , GotoElemC

    -- * Goto Result Types
  , GotoResult(..)
  , SomeGoto(..)

    -- * Effect Interpretation
  , runGotoCapture
  , runGotoIgnore
  ) where

import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy(..))
import Data.Dynamic (Dynamic, toDyn, Typeable)
import Data.Text (Text)
import qualified Data.Text as T
import Effectful hiding (inject)
import Effectful.Dispatch.Dynamic
import qualified Effectful.State.Static.Local as EState
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal, TypeError, ErrorMessage(..))

import Tidepool.Graph.Types (Exit, Self)

-- ════════════════════════════════════════════════════════════════════════════
-- ONEOF: TYPE-INDEXED SUM TYPE
-- ════════════════════════════════════════════════════════════════════════════

-- | A type-indexed sum type: "one of these types".
--
-- Position in the list encodes which type was chosen. Pattern matching is
-- fully typed - each case knows the exact payload type.
--
-- Note: @OneOf '[]@ is uninhabited (has no constructors). Use 'NonEmptyList'
-- constraint if you need to enforce non-empty lists at compile time.
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
  Here  :: t -> OneOf (t ': ts)
  -- | The value is somewhere in the rest of the list
  There :: OneOf ts -> OneOf (t ': ts)

-- | Constraint that ensures a type list is non-empty.
--
-- Use this to catch empty 'OneOf' or 'GotoChoice' at compile time:
--
-- @
-- myFunction :: NonEmptyList targets => GotoChoice targets -> ...
-- @
type NonEmptyList :: [Type] -> Constraint
type family NonEmptyList ts where
  NonEmptyList '[] = TypeError
    ('Text "Empty type list is not allowed"
     ':$$: 'Text ""
     ':$$: 'Text "OneOf '[] and GotoChoice '[] have no valid constructors."
     ':$$: 'Text "Ensure your target list contains at least one transition."
    )
  NonEmptyList (_ ': _) = ()

-- | Inject a value into a 'OneOf' at its position in the type list.
--
-- This typeclass finds the correct position automatically:
--
-- @
-- inject @Int @'[Int, String] 42 = Here 42
-- inject @String @'[Int, String] "hi" = There (Here "hi")
-- @
class Inject (t :: Type) (ts :: [Type]) where
  inject :: t -> OneOf ts

-- | Base case: t is at the head of the list
instance {-# OVERLAPPING #-} Inject t (t ': ts) where
  inject = Here

-- | Inductive case: t is somewhere in the tail
instance Inject t ts => Inject t (t' ': ts) where
  inject = There . inject

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

-- | Inject a target's payload into a 'OneOf' at the correct position.
--
-- Unlike 'Inject', this matches on the full 'To name payload' marker,
-- correctly handling cases where multiple targets have the same payload type.
--
-- @
-- injectTarget @(To "a" Int) 42 -- injects at position of To "a" Int
-- injectTarget @(To "b" Int) 42 -- injects at position of To "b" Int (different!)
-- @
class InjectTarget (target :: Type) (targets :: [Type]) where
  injectTarget :: PayloadOf target -> OneOf (Payloads targets)

-- | Base case: target is at the head
instance {-# OVERLAPPING #-} InjectTarget (To name payload) (To name payload ': ts) where
  injectTarget = Here

-- | Inductive case: target is in the tail
--
-- Note: We match on @To name' payload'@ specifically so GHC can reduce
-- @Payloads (To name' payload' : ts)@ to @payload' : Payloads ts@.
instance InjectTarget target ts => InjectTarget target (To name' payload' ': ts) where
  injectTarget = There . injectTarget @target @ts

-- ════════════════════════════════════════════════════════════════════════════
-- THE GOTO EFFECT
-- ════════════════════════════════════════════════════════════════════════════

-- | Effect for transitioning to a graph node.
--
-- The @target@ parameter can be:
--
-- * A type-level 'Symbol' (e.g., @"nodeName"@) for transitions to named nodes
-- * 'Exit' for terminating the graph with a result
--
-- The @payload@ type is the data passed to the target node, which must
-- match what the target 'Needs'.
type Goto :: k -> Type -> Effect
data Goto target payload :: Effect where
  GotoOp :: a -> Goto target a m ()

type instance DispatchOf (Goto target a) = 'Dynamic

-- | Perform a transition to the specified target.
--
-- @
-- -- Transition to a named node
-- goto @"nextNode" somePayload
--
-- -- Exit the graph
-- goto @Exit finalResult
-- @
goto :: forall {k} (target :: k) a es. Goto target a :> es => a -> Eff es ()
goto x = send (GotoOp x :: Goto target a (Eff es) ())

-- ════════════════════════════════════════════════════════════════════════════
-- GOTOCHOICE RETURN TYPE
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

-- | Return type for handlers that must choose a transition.
--
-- Unlike the 'Goto' effect which is fired mid-execution, 'GotoChoice' is
-- returned by the handler to guarantee that a transition is always selected.
--
-- The @targets@ parameter is a type-level list of 'To' markers:
-- @'[To "nodeA" PayloadA, To "nodeB" PayloadB, To Exit Response]@
--
-- Internally, 'GotoChoice' is a newtype over 'OneOf', enabling fully typed
-- dispatch without existentials or unsafeCoerce.
--
-- = Example
--
-- @
-- routerHandler :: Intent -> Eff es (GotoChoice '[To "refund" Msg, To Exit Response])
-- routerHandler intent = case intent of
--   RefundIntent -> pure $ gotoChoice @"refund" msg
--   DoneIntent   -> pure $ gotoExit response
-- @
type GotoChoice :: [Type] -> Type
newtype GotoChoice targets = GotoChoice { unGotoChoice :: OneOf (Payloads targets) }

-- | Check if a To marker is in the targets list (returns Bool).
type GotoElem :: Type -> [Type] -> Bool
type family GotoElem g targets where
  GotoElem _ '[] = 'False
  GotoElem g (g ': _) = 'True
  GotoElem g (_ ': rest) = GotoElem g rest

-- | Constraint version of 'GotoElem' with helpful error message.
type GotoElemC :: Type -> [Type] -> Constraint
type family GotoElemC g targets where
  GotoElemC g targets =
    GotoElemC' g targets (GotoElem g targets)

type GotoElemC' :: Type -> [Type] -> Bool -> Constraint
type family GotoElemC' g targets result where
  GotoElemC' _ _ 'True = ()
  GotoElemC' g targets 'False = TypeError
    ( 'Text "Invalid transition target"
      ':$$: 'Text ""
      ':$$: 'Text "The transition:"
      ':$$: 'Text "  " ':<>: 'ShowType g
      ':$$: 'Text ""
      ':$$: 'Text "is not in the allowed targets:"
      ':$$: 'Text "  " ':<>: 'ShowType targets
    )

-- | Construct a 'GotoChoice' for a named node target.
--
-- @
-- gotoChoice @"nextNode" payload
-- @
gotoChoice
  :: forall (name :: Symbol) payload targets.
     ( NonEmptyList targets
     , InjectTarget (To name payload) targets
     , GotoElemC (To name payload) targets
     )
  => payload -> GotoChoice targets
gotoChoice payload = GotoChoice (injectTarget @(To name payload) @targets payload)

-- | Construct a 'GotoChoice' for exiting the graph.
--
-- @
-- gotoExit response
-- @
gotoExit
  :: forall payload targets.
     ( NonEmptyList targets
     , InjectTarget (To Exit payload) targets
     , GotoElemC (To Exit payload) targets
     )
  => payload -> GotoChoice targets
gotoExit payload = GotoChoice (injectTarget @(To Exit payload) @targets payload)

-- | Construct a 'GotoChoice' for a self-loop.
--
-- @
-- gotoSelf updatedState
-- @
gotoSelf
  :: forall payload targets.
     ( NonEmptyList targets
     , InjectTarget (To Self payload) targets
     , GotoElemC (To Self payload) targets
     )
  => payload -> GotoChoice targets
gotoSelf payload = GotoChoice (injectTarget @(To Self payload) @targets payload)

-- Note: gotoChoiceToResult has been removed. The typed executor in Execute.hs
-- dispatches directly on OneOf without needing to convert to Dynamic-based GotoResult.

-- ════════════════════════════════════════════════════════════════════════════
-- LLM HANDLER VARIANTS
-- ════════════════════════════════════════════════════════════════════════════

-- | Handler variants for LLM nodes.
--
-- LLM nodes can have before-only, after-only, or both handlers:
--
-- * 'LLMBefore': Provides template context before LLM call, uses Needs-based data flow
-- * 'LLMAfter': Routes based on LLM output, uses default context
-- * 'LLMBoth': Custom context AND explicit routing
--
-- The constructor explicitly declares which phases are handled, making the
-- handler type self-documenting.
--
-- = Examples
--
-- @
-- -- Before-only: classifier with custom context
-- sgClassify :: LLMHandler Message Intent '[] es ClassifyContext
-- sgClassify = LLMBefore $ \\msg -> pure ClassifyContext { topic = msg.content }
--
-- -- After-only: router using LLM output (no access to before-phase data)
-- sgRouter :: LLMHandler () Intent '[To "refund" Intent, To "faq" Intent] es ()
-- sgRouter = LLMAfter $ \\intent -> pure $ case intent of
--   IntentRefund -> gotoChoice @"refund" intent
--   IntentFaq    -> gotoChoice @"faq" intent
--
-- -- Both: custom context AND explicit routing
-- sgSmart :: LLMHandler Message Intent '[To "a" X, To "b" Y] es SmartContext
-- sgSmart = LLMBoth
--   (\\msg -> pure SmartContext { ... })
--   (\\intent -> pure $ gotoChoice @"a" (processIntent intent))
-- @
type LLMHandler :: Type -> Type -> [Type] -> [Effect] -> Type -> Type
data LLMHandler needs schema targets es tpl where
  -- | Before-only: provides template context, uses Needs-based data flow after LLM
  LLMBefore
    :: forall tpl needs schema es.
       (needs -> Eff es tpl)
    -> LLMHandler needs schema '[] es tpl

  -- | After-only: uses default context, explicit routing based on LLM output
  LLMAfter
    :: forall needs schema targets es.
       (schema -> Eff es (GotoChoice targets))
    -> LLMHandler needs schema targets es ()

  -- | Both: custom context AND explicit routing
  LLMBoth
    :: forall tpl needs schema targets es.
       (needs -> Eff es tpl)
    -> (schema -> Eff es (GotoChoice targets))
    -> LLMHandler needs schema targets es tpl

-- ════════════════════════════════════════════════════════════════════════════
-- GOTO RESULT TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Result of capturing a Goto from a Logic node handler.
--
-- This is used by the graph runner to determine which node to execute next.
--
-- __DEPRECATED__: Use 'GotoChoice' with 'DispatchGoto' for type-safe dispatch.
-- This type uses 'Dynamic' which loses type safety at the boundary.
data GotoResult
  = GotoNode Text Dynamic   -- ^ Transition to named node with payload
  | GotoExit Dynamic        -- ^ Exit graph with result
  | GotoSelf Dynamic        -- ^ Self-loop with updated state
  deriving (Show)

{-# DEPRECATED GotoResult "Use GotoChoice with DispatchGoto for type-safe dispatch" #-}
{-# DEPRECATED GotoNode "Use gotoChoice @name from GotoChoice API instead" #-}
{-# DEPRECATED GotoExit "Use gotoExit from GotoChoice API instead" #-}
{-# DEPRECATED GotoSelf "Use gotoSelf from GotoChoice API instead" #-}

-- | Existential wrapper for any Goto effect.
--
-- Used internally by the runner to handle multiple Goto effects uniformly.
--
-- __DEPRECATED__: Use 'GotoChoice' with 'DispatchGoto' for type-safe dispatch.
data SomeGoto where
  SomeGotoNode
    :: forall (name :: Symbol) a.
       (KnownSymbol name, Typeable a)
    => Proxy name -> a -> SomeGoto
  SomeGotoExit
    :: forall a. Typeable a
    => a -> SomeGoto
  SomeGotoSelf
    :: forall a. Typeable a
    => a -> SomeGoto

{-# DEPRECATED SomeGoto "Use GotoChoice with DispatchGoto for type-safe dispatch" #-}

-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT INTERPRETATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Run a Goto effect by capturing the transition.
--
-- Returns the payload wrapped in a 'GotoResult' for the runner to process.
-- This is the primary interpreter used during graph execution.
--
-- __DEPRECATED__: Use handlers that return 'GotoChoice' and dispatch with
-- 'DispatchGoto' instead. This approach loses type safety via 'Dynamic'.
--
-- @
-- result <- runGotoCapture @"target" @Payload $ do
--   goto @"target" somePayload
-- case result of
--   Just (GotoNode name dyn) -> -- handle transition
--   Just (GotoExit dyn) -> -- handle exit
--   Nothing -> -- no goto was called (shouldn't happen for Logic nodes)
-- @
{-# DEPRECATED runGotoCapture "Use GotoChoice return type with DispatchGoto instead" #-}
runGotoCapture
  :: forall name a es b.
     (KnownSymbol name, Typeable a)
  => Eff (Goto name a : es) b
  -> Eff es (Maybe GotoResult, b)
runGotoCapture action = do
  (b, mResult) <- reinterpret (EState.runState Nothing) handler action
  pure (mResult, b)
  where
    handler :: forall es'. EState.State (Maybe GotoResult) :> es'
            => EffectHandler (Goto name a) es'
    handler _ = \case
      GotoOp payload -> do
        let target = T.pack $ symbolVal (Proxy @name)
        EState.put (Just $ GotoNode target (toDyn payload))

-- | Run a Goto effect by ignoring it (for testing/debugging).
runGotoIgnore
  :: forall name a es b.
     Eff (Goto name a : es) b
  -> Eff es b
runGotoIgnore = interpret $ \_ -> \case
  GotoOp _ -> pure ()

-- | Specialized runner for Goto Exit
--
-- __DEPRECATED__: Use 'gotoExit' with 'GotoChoice' return type instead.
{-# DEPRECATED runGotoExitCapture "Use GotoChoice return type with DispatchGoto instead" #-}
runGotoExitCapture
  :: forall a es b.
     Typeable a
  => Eff (Goto Exit a : es) b
  -> Eff es (Maybe GotoResult, b)
runGotoExitCapture action = do
  (b, mResult) <- reinterpret (EState.runState Nothing) handler action
  pure (mResult, b)
  where
    handler :: forall es'. EState.State (Maybe GotoResult) :> es'
            => EffectHandler (Goto Exit a) es'
    handler _ = \case
      GotoOp payload -> EState.put (Just $ GotoExit (toDyn payload))

-- Note: The full runner will need to handle multiple Goto effects in a single
-- node's effect stack. This is done by the graph Runner, which composes
-- multiple runGotoCapture calls or uses a more sophisticated approach.
--
-- For now, individual Goto effects can be captured one at a time. The Runner
-- will need to:
-- 1. Run the handler's effects
-- 2. Capture which Goto was actually called (only one should be)
-- 3. Extract the payload and target for the next node
