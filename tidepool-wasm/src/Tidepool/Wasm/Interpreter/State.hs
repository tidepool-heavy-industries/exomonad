{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | State effect interpreter for WASM.
--
-- This module provides @runStateAsYield@, which interprets the @State@ effect
-- by yielding @SerializableEffect@ to TypeScript.
--
-- = Design
--
-- The interpreter converts:
--
-- * @Get@ → @EffGetState key@, parses result from JSON
-- * @Put s@ → @EffSetState key (toJSON s)@, fire-and-forget
--
-- = State Key
--
-- Currently uses "graphState" as the default key. In production, this could
-- be parameterized per-graph or per-node.
--
-- = Type Safety
--
-- The state type @s@ must have @ToJSON@ and @FromJSON@ instances.
-- State is serialized to JSON for transport across the FFI boundary.
module Tidepool.Wasm.Interpreter.State
  ( runStateAsYield
  , runStateAsYieldWith
  ) where

import Control.Monad.Freer (Eff, Member, interpret)
import Control.Monad.Freer.Coroutine (Yield, yield)
import Data.Aeson (ToJSON(..), FromJSON(..), fromJSON, Result(..))
import Data.Text (Text)

import Tidepool.Effect.Types (State(..))
import Tidepool.Wasm.WireTypes
  ( SerializableEffect(..)
  , EffectResult(..)
  )


-- | Interpret @State s@ effect by yielding to TypeScript.
--
-- Uses "graphState" as the default state key.
-- The state type @s@ must have @ToJSON@ and @FromJSON@ instances.
--
-- State is stored in the Durable Object and persists across FFI calls.
runStateAsYield
  :: forall s effs a.
     ( ToJSON s
     , FromJSON s
     , Member (Yield SerializableEffect EffectResult) effs
     )
  => s  -- ^ Initial state (used if storage is empty)
  -> Eff (State s ': effs) a
  -> Eff effs a
runStateAsYield = runStateAsYieldWith "graphState"


-- | Interpret @State s@ effect with a custom state key.
--
-- Use this when you need multiple state stores or custom key names.
runStateAsYieldWith
  :: forall s effs a.
     ( ToJSON s
     , FromJSON s
     , Member (Yield SerializableEffect EffectResult) effs
     )
  => Text  -- ^ State key (e.g., "worldState", "sessionState")
  -> s     -- ^ Initial state (used if storage is empty)
  -> Eff (State s ': effs) a
  -> Eff effs a
runStateAsYieldWith key initial = interpret (handleState key initial)
  where
    handleState :: Text -> s
                -> State s x -> Eff effs x
    handleState stateKey defaultState = \case
      Get -> do
        result <- yield (EffGetState stateKey) (id @EffectResult)
        pure $ case result of
          ResSuccess (Just val) -> case fromJSON val of
            Success s -> s
            Error _   -> defaultState  -- Parse failed, use default
          ResSuccess Nothing -> defaultState   -- No stored state
          ResError _         -> defaultState   -- Error, use default

      Put s -> do
        _ <- yield (EffSetState stateKey (toJSON s)) (id @EffectResult)
        pure ()
