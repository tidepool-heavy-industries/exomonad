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

    -- * GotoChoice Return Type
  , To
  , GotoChoice(..)
  , gotoChoice
  , gotoExit
  , gotoSelf
  , gotoChoiceToResult

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
import Effectful
import Effectful.Dispatch.Dynamic
import qualified Effectful.State.Static.Local as EState
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal, TypeError, ErrorMessage(..))

import Tidepool.Graph.Types (Exit, Self)

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
-- = Example
--
-- @
-- routerHandler :: Intent -> Eff es (GotoChoice '[To "refund" Msg, To Exit Response])
-- routerHandler intent = case intent of
--   RefundIntent -> pure $ gotoChoice @"refund" msg
--   DoneIntent   -> pure $ gotoExit response
-- @
type GotoChoice :: [Type] -> Type
data GotoChoice targets where
  -- | Transition to a named node
  GotoChoiceNode
    :: forall (name :: Symbol) payload targets.
       ( KnownSymbol name
       , Typeable payload
       , GotoElemC (To name payload) targets
       )
    => Proxy name -> payload -> GotoChoice targets

  -- | Exit the graph with a result
  GotoChoiceExit
    :: forall payload targets.
       ( Typeable payload
       , GotoElemC (To Exit payload) targets
       )
    => payload -> GotoChoice targets

  -- | Self-loop back to current node
  GotoChoiceSelf
    :: forall payload targets.
       ( Typeable payload
       , GotoElemC (To Self payload) targets
       )
    => payload -> GotoChoice targets

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
     ( KnownSymbol name
     , Typeable payload
     , GotoElemC (To name payload) targets
     )
  => payload -> GotoChoice targets
gotoChoice = GotoChoiceNode (Proxy @name)

-- | Construct a 'GotoChoice' for exiting the graph.
--
-- @
-- gotoExit response
-- @
gotoExit
  :: forall payload targets.
     ( Typeable payload
     , GotoElemC (To Exit payload) targets
     )
  => payload -> GotoChoice targets
gotoExit = GotoChoiceExit

-- | Construct a 'GotoChoice' for a self-loop.
--
-- @
-- gotoSelf updatedState
-- @
gotoSelf
  :: forall payload targets.
     ( Typeable payload
     , GotoElemC (To Self payload) targets
     )
  => payload -> GotoChoice targets
gotoSelf = GotoChoiceSelf

-- | Convert a 'GotoChoice' to a 'GotoResult' for the runner.
gotoChoiceToResult :: GotoChoice targets -> GotoResult
gotoChoiceToResult (GotoChoiceNode proxy payload) =
  GotoNode (T.pack $ symbolVal proxy) (toDyn payload)
gotoChoiceToResult (GotoChoiceExit payload) =
  GotoExit (toDyn payload)
gotoChoiceToResult (GotoChoiceSelf payload) =
  GotoSelf (toDyn payload)

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
data GotoResult
  = GotoNode Text Dynamic   -- ^ Transition to named node with payload
  | GotoExit Dynamic        -- ^ Exit graph with result
  | GotoSelf Dynamic        -- ^ Self-loop with updated state
  deriving (Show)

-- | Existential wrapper for any Goto effect.
--
-- Used internally by the runner to handle multiple Goto effects uniformly.
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

-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT INTERPRETATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Run a Goto effect by capturing the transition.
--
-- Returns the payload wrapped in a 'GotoResult' for the runner to process.
-- This is the primary interpreter used during graph execution.
--
-- @
-- result <- runGotoCapture @"target" @Payload $ do
--   goto @"target" somePayload
-- case result of
--   Just (GotoNode name dyn) -> -- handle transition
--   Just (GotoExit dyn) -> -- handle exit
--   Nothing -> -- no goto was called (shouldn't happen for Logic nodes)
-- @
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
