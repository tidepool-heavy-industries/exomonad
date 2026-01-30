{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
-- Currently uses 'defaultStateKey' as the default key. In production, this could
-- be parameterized per-graph or per-node.
--
-- = Type Safety
--
-- The state type @s@ must have @ToJSON@ and @FromJSON@ instances.
-- State is serialized to JSON for transport across the FFI boundary.
module ExoMonad.Wasm.Interpreter.State
  ( runStateAsYield,
    runStateAsYieldWith,
  )
where

import Control.Monad.Freer (Eff, Member, interpret)
import Control.Monad.Freer.Coroutine (Yield, yield)
import Data.Aeson (FromJSON (..), Result (..), ToJSON (..), fromJSON)
import Data.Text (Text)
import ExoMonad.Effect.Types (State (..))
import ExoMonad.Wasm.WireTypes
  ( EffectResult (..),
    SerializableEffect (..),
  )

-- | Default state key used when no custom key is provided.
--
-- In production, this could be parameterized per-graph or per-node.
defaultStateKey :: Text
defaultStateKey = "graphState"

-- | Interpret @State s@ effect by yielding to TypeScript.
--
-- Uses 'defaultStateKey' as the default state key.
-- The state type @s@ must have @ToJSON@ and @FromJSON@ instances.
--
-- State is stored in the Durable Object and persists across FFI calls.
runStateAsYield ::
  forall s effs a.
  ( ToJSON s,
    FromJSON s,
    Member (Yield SerializableEffect EffectResult) effs
  ) =>
  -- | Initial state (used if storage is empty)
  s ->
  Eff (State s ': effs) a ->
  Eff effs a
runStateAsYield = runStateAsYieldWith defaultStateKey

-- | Interpret @State s@ effect with a custom state key.
--
-- Use this when you need multiple state stores or custom key names.
runStateAsYieldWith ::
  forall s effs a.
  ( ToJSON s,
    FromJSON s,
    Member (Yield SerializableEffect EffectResult) effs
  ) =>
  -- | State key (e.g., "worldState", "sessionState")
  Text ->
  -- | Initial state (used if storage is empty)
  s ->
  Eff (State s ': effs) a ->
  Eff effs a
runStateAsYieldWith key initial = interpret (handleState key initial)
  where
    handleState ::
      Text ->
      s ->
      State s x ->
      Eff effs x
    handleState stateKey defaultState = \case
      Get -> do
        result <- yield (EffGetState stateKey) (id @EffectResult)
        pure $ case result of
          ResSuccess (Just val) -> case fromJSON val of
            Success s -> s
            Error _ -> defaultState -- Parse failed, use default
          ResSuccess Nothing -> defaultState -- No stored state
          ResError _ -> defaultState -- Error, use default
      Put s -> do
        _ <- yield (EffSetState stateKey (toJSON s)) (id @EffectResult)
        pure ()
