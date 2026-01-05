{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- Note: -Wno-incomplete-patterns is needed because unwrapSingleChoice has a pattern
-- that IS exhaustive for single-element OneOf lists, but GHC can't prove this.
-- By suppressing the warning here, we absorb it so users don't see it.

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
-- gRouter :: mode :- G.LogicNode
--     :@ Needs '[Message, Intent]
--     :@ UsesEffects '[
--         Goto "gRefund" Message     -- Can transition to refund node
--       , Goto "gSupport" Message    -- Or to support node
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

    -- * OneOf Sum Type (constructors hidden - use smart constructors)
  , OneOf       -- Type only, constructors in Goto.Internal
  , NonEmptyList
  , Payloads
  , PayloadOf
  , InjectTarget(..)
  -- Note: Inject has been removed from exports. It uses type equality which
  -- finds the first match in duplicate-type lists. Use InjectTarget instead,
  -- which matches on full (To name payload) markers for correct positioning.

    -- * GotoChoice Return Type (constructor hidden - use smart constructors)
  , To
  , GotoChoice  -- Type only, constructor in Goto.Internal
  , gotoChoice
  , gotoExit
  , gotoSelf
  , unwrapSingleChoice

    -- * LLM Handler Variants
  , LLMHandler(..)

    -- * Target Validation
  , GotoElem
  , GotoElemC

    -- * Goto Result Types (deprecated)
  , GotoResult(..)
  , SomeGoto(..)

    -- * Effect Interpretation (deprecated)
  , runGotoCapture
  , runGotoIgnore
  ) where

import Data.Aeson (ToJSON(..))
import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy(..))
import Data.Dynamic (Dynamic, toDyn, Typeable)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Freer (Eff, Member, send, interpret)
import Control.Monad.Freer.Internal (handleRelayS)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal, TypeError, ErrorMessage(..))
import Tidepool.Graph.Errors
  ( HR, Blank, WhatHappened, HowItWorks, Fixes, Example
  , Indent, CodeLine, Bullet, FormatTargetList
  )
import Text.Ginger.TH (TypedTemplate)
import Text.Parsec.Pos (SourcePos)

import Tidepool.Graph.Types (Exit, Self)

-- Import from Internal (re-exports types, we hide constructors in this module's exports)
import Tidepool.Graph.Goto.Internal (OneOf(..), GotoChoice(..), To, Payloads, PayloadOf)

-- ════════════════════════════════════════════════════════════════════════════
-- ONEOF CONSTRAINTS AND INJECTION
-- ════════════════════════════════════════════════════════════════════════════

-- Note: OneOf, GotoChoice, To, Payloads, PayloadOf are defined in
-- Tidepool.Graph.Goto.Internal and imported above.

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
    ( HR
      ':$$: 'Text "  Handler has no exit points (empty target list)"
      ':$$: HR
      ':$$: Blank
      ':$$: WhatHappened
      ':$$: Indent "Your handler returns GotoChoice '[]"
      ':$$: Indent "This type has NO constructors - there's no way to return a value!"
      ':$$: Blank
      ':$$: HowItWorks
      ':$$: Indent "Every handler must be able to exit. The type system enforces"
      ':$$: Indent "this by requiring at least one target in GotoChoice:"
      ':$$: Blank
      ':$$: CodeLine "GotoChoice '[]                               -- Impossible!"
      ':$$: CodeLine "GotoChoice '[To Exit Result]                 -- Can exit"
      ':$$: CodeLine "GotoChoice '[To \"next\" X, To Exit Result]    -- Can continue OR exit"
      ':$$: Blank
      ':$$: Indent "This is like requiring every code path to return a value."
      ':$$: Indent "Dead ends are compile errors, not runtime mysteries."
      ':$$: Blank
      ':$$: Fixes
      ':$$: Bullet "Add Goto Exit to UsesEffects: UsesEffects '[Goto Exit YourResultType]"
      ':$$: Bullet "Add a transition: UsesEffects '[Goto \"nextNode\" Payload]"
      ':$$: Bullet "Check that you haven't accidentally filtered out all targets"
      ':$$: Blank
      ':$$: Example
      ':$$: CodeLine "-- A handler that can either continue or exit:"
      ':$$: CodeLine "router :: Intent -> Eff es (GotoChoice '[To \"process\" Data, To Exit Done])"
      ':$$: CodeLine "router intent = case intent of"
      ':$$: CodeLine "  NeedMore x -> pure $ gotoChoice @\"process\" x"
      ':$$: CodeLine "  Finished   -> pure $ gotoExit Done"
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
data Goto (target :: k) (payload :: Type) r where
  GotoOp :: a -> Goto target a ()

-- | Perform a transition to the specified target.
--
-- @
-- -- Transition to a named node
-- goto @"nextNode" somePayload
--
-- -- Exit the graph
-- goto @Exit finalResult
-- @
goto :: forall {k} (target :: k) a effs. Member (Goto target a) effs => a -> Eff effs ()
goto x = send (GotoOp x :: Goto target a ())

-- ════════════════════════════════════════════════════════════════════════════
-- JSON SERIALIZATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Serialize a OneOf by serializing whatever payload it contains.
--
-- This flattens the sum type - we just serialize the inner value.
-- The position information is lost, but for WASM FFI we only care about
-- the payload reaching the exit point.
instance {-# OVERLAPPING #-} ToJSON t => ToJSON (OneOf '[t]) where
  toJSON (Here x) = toJSON x

instance {-# OVERLAPPABLE #-} (ToJSON t, ToJSON (OneOf ts)) => ToJSON (OneOf (t ': ts)) where
  toJSON (Here x) = toJSON x
  toJSON (There rest) = toJSON rest

-- | Serialize a GotoChoice by serializing its inner OneOf.
instance ToJSON (OneOf (Payloads targets)) => ToJSON (GotoChoice targets) where
  toJSON (GotoChoice oneOf) = toJSON oneOf


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
  GotoElemC' (To (name :: Symbol) payload) targets 'False = TypeError
    ( HR
      ':$$: 'Text "  Unknown transition target: \"" ':<>: 'Text name ':<>: 'Text "\""
      ':$$: HR
      ':$$: Blank
      ':$$: WhatHappened
      ':$$: Indent "You wrote:"
      ':$$: CodeLine "gotoChoice @\"" ':<>: 'Text name ':<>: 'Text "\" ..."
      ':$$: Blank
      ':$$: Indent "But this target is not in your handler's allowed transitions."
      ':$$: Blank
      ':$$: Indent "Valid targets for this handler:"
      ':$$: FormatTargetList targets
      ':$$: Blank
      ':$$: HowItWorks
      ':$$: Indent "Handlers declare their possible exits via UsesEffects:"
      ':$$: Blank
      ':$$: CodeLine "myHandler :: X -> Eff es (GotoChoice '[To \"a\" A, To Exit B])"
      ':$$: CodeLine "                                      ^^^^^^^^^^^^^^^^^^^^^^^"
      ':$$: CodeLine "                                      Only THESE targets are valid"
      ':$$: Blank
      ':$$: Indent "The type checker ensures you can only transition to declared"
      ':$$: Indent "targets. This prevents runtime \"node not found\" errors."
      ':$$: Blank
      ':$$: Fixes
      ':$$: Bullet "Check spelling of the target name"
      ':$$: Bullet "Add the target: UsesEffects '[Goto \"" ':<>: 'Text name ':<>: 'Text "\" " ':<>: 'ShowType payload ':<>: 'Text ", ...]"
      ':$$: Bullet "Use gotoExit if you meant to exit the graph"
      ':$$: Blank
      ':$$: Example
      ':$$: CodeLine "-- Your handler signature declares valid targets:"
      ':$$: CodeLine "router :: Intent -> Eff es (GotoChoice '[To \"refund\" Msg, To Exit Done])"
      ':$$: CodeLine ""
      ':$$: CodeLine "-- Then you can only use those targets:"
      ':$$: CodeLine "router intent = case intent of"
      ':$$: CodeLine "  Refund -> pure $ gotoChoice @\"refund\" msg  -- OK: declared above"
      ':$$: CodeLine "  Done   -> pure $ gotoExit done              -- OK: Exit declared"
      ':$$: CodeLine "  Other  -> pure $ gotoChoice @\"faq\" msg     -- ERROR: not declared!"
    )
  -- Fallback for non-To types (shouldn't happen in practice)
  GotoElemC' g targets 'False = TypeError
    ( 'Text "Invalid transition target: " ':<>: 'ShowType g
      ':$$: 'Text "Expected a To marker but got something else."
      ':$$: 'Text "Valid targets: " ':<>: 'ShowType targets
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

-- | Extract the payload from a single-target 'GotoChoice'.
--
-- This is useful for exit-only handlers where you need to extract the result
-- without pattern matching (which would trigger incomplete pattern warnings
-- even though the pattern is actually exhaustive for single-element lists).
--
-- @
-- -- Handler that can only exit
-- exitOnlyHandler :: Input -> Eff es (GotoChoice '[To Exit Response])
-- exitOnlyHandler input = pure $ gotoExit (processInput input)
--
-- -- Extract the response without pattern matching
-- let response = unwrapSingleChoice choice  -- :: Response
-- @
--
-- Note: This only works for single-target 'GotoChoice' types. For multi-target
-- choices, use pattern matching via "Tidepool.Graph.Goto.Internal" or dispatch
-- via 'DispatchGoto'.
unwrapSingleChoice :: GotoChoice '[To name payload] -> payload
unwrapSingleChoice (GotoChoice (Here p)) = p

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
-- sgClassify :: LLMHandler Message Intent '[] effs ClassifyContext
-- sgClassify = LLMBefore $ \\msg -> pure ClassifyContext { topic = msg.content }
--
-- -- After-only: router using LLM output (no access to before-phase data)
-- sgRouter :: LLMHandler () Intent '[To "refund" Intent, To "faq" Intent] effs ()
-- sgRouter = LLMAfter $ \\intent -> pure $ case intent of
--   IntentRefund -> gotoChoice @"refund" intent
--   IntentFaq    -> gotoChoice @"faq" intent
--
-- -- Both: custom context AND explicit routing (with templates)
-- sgSmart :: LLMHandler Message Intent '[To "a" X, To "b" Y] effs SmartContext
-- sgSmart = LLMBoth
--   Nothing                              -- no system template
--   (templateCompiled @SmartTpl)         -- user template
--   (\\msg -> pure SmartContext { ... }) -- context builder
--   (\\intent -> pure $ gotoChoice @"a" (processIntent intent))  -- router
-- @
type LLMHandler :: Type -> Type -> [Type] -> [Type -> Type] -> Type -> Type
data LLMHandler needs schema targets effs tpl where
  -- | Before-only: provides template context, uses Needs-based data flow after LLM
  LLMBefore
    :: forall tpl needs schema effs.
       (needs -> Eff effs tpl)
    -> LLMHandler needs schema '[] effs tpl

  -- | After-only: uses default context, explicit routing based on LLM output
  LLMAfter
    :: forall needs schema targets effs.
       (schema -> Eff effs (GotoChoice targets))
    -> LLMHandler needs schema targets effs ()

  -- | Both: custom context AND explicit routing
  --
  -- Takes optional system template, required user template, before handler, and after handler.
  -- Both templates share the same context type (tpl).
  LLMBoth
    :: forall tpl needs schema targets effs.
       Maybe (TypedTemplate tpl SourcePos)      -- ^ Optional system prompt template
    -> TypedTemplate tpl SourcePos              -- ^ User prompt template (required)
    -> (needs -> Eff effs tpl)                  -- ^ Builds context for both templates
    -> (schema -> Eff effs (GotoChoice targets))  -- ^ Routes based on LLM output
    -> LLMHandler needs schema targets effs tpl

-- ════════════════════════════════════════════════════════════════════════════
-- GOTO RESULT TYPES (DEPRECATED)
-- ════════════════════════════════════════════════════════════════════════════

-- | Result of capturing a Goto from a Logic node handler.
--
-- This is used by the graph runner to determine which node to execute next.
--
-- __DEPRECATED__: Use 'GotoChoice' with 'DispatchGoto' for type-safe dispatch.
-- This type uses 'Dynamic' which loses type safety at the boundary.
data GotoResult
  = GotoNode Text Dynamic   -- ^ Transition to named node with payload
  | GotoExit' Dynamic       -- ^ Exit graph with result
  | GotoSelf' Dynamic       -- ^ Self-loop with updated state
  deriving (Show)

{-# DEPRECATED GotoResult "Use GotoChoice with DispatchGoto for type-safe dispatch" #-}
{-# DEPRECATED GotoNode "Use gotoChoice @name from GotoChoice API instead" #-}

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
-- EFFECT INTERPRETATION (DEPRECATED)
-- ════════════════════════════════════════════════════════════════════════════

-- | Run a Goto effect by capturing the transition.
--
-- __DEPRECATED__: Use handlers that return 'GotoChoice' and dispatch with
-- 'DispatchGoto' instead. This approach loses type safety via 'Dynamic'.
{-# DEPRECATED runGotoCapture "Use GotoChoice return type with DispatchGoto instead" #-}
runGotoCapture
  :: forall name a effs b.
     (KnownSymbol name, Typeable a)
  => Eff (Goto name a ': effs) b
  -> Eff effs (Maybe GotoResult, b)
runGotoCapture = handleRelayS Nothing (\s a -> pure (s, a)) $ \s -> \case
  GotoOp payload -> \k -> do
    let target = T.pack $ symbolVal (Proxy @name)
    k (Just $ GotoNode target (toDyn payload)) ()

-- | Run a Goto effect by ignoring it (for testing/debugging).
runGotoIgnore
  :: forall name a effs b.
     Eff (Goto name a ': effs) b
  -> Eff effs b
runGotoIgnore = interpret $ \case
  GotoOp _ -> pure ()
