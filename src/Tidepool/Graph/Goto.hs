{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
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

    -- * Goto Result Types
  , GotoResult(..)
  , SomeGoto(..)

    -- * Effect Interpretation
  , runGotoCapture
  , runGotoIgnore
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Dynamic (Dynamic, toDyn, Typeable)
import Data.Text (Text)
import qualified Data.Text as T
import Effectful
import Effectful.Dispatch.Dynamic
import qualified Effectful.State.Static.Local as EState
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)

import Tidepool.Graph.Types (Exit)

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
-- GOTO RESULT TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Result of capturing a Goto from a Logic node handler.
--
-- This is used by the graph runner to determine which node to execute next.
data GotoResult
  = GotoNode Text Dynamic   -- ^ Transition to named node with payload
  | GotoExit Dynamic        -- ^ Exit graph with result
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
