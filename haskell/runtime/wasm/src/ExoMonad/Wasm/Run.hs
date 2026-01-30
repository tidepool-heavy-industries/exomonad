{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | High-level runner for portable handlers in WASM.
--
-- This module provides @runPortableHandler@, which runs handlers that use
-- standard exomonad-core effects (@LLM@, @Log@, @State@) in WASM contexts.
--
-- = Portable Handlers
--
-- A "portable handler" is one that uses effect constraints rather than
-- depending on a specific effect stack:
--
-- @
-- -- Portable: works in WASM and native
-- myHandler :: (Member LLM effs, Member Log effs) => Input -> Eff effs Output
--
-- -- Non-portable: tied to WasmM
-- myHandler :: Input -> WasmM Output
-- @
--
-- = Usage
--
-- @
-- -- Define portable handler
-- processMessage :: (Member LLM effs, Member Log effs)
--                => Message -> Eff effs Response
-- processMessage msg = do
--   logInfo "Processing message"
--   result <- runTurn systemPrompt [TextBlock msg.content] schema tools
--   case result of
--     TurnCompleted (TurnParsed tr) -> pure (Response tr.trOutput)
--     _ -> pure (ErrorResponse "LLM failed")
--
-- -- Run in WASM
-- import ExoMonad.Wasm.Run (runPortableHandler)
-- import ExoMonad.Wasm.Runner (initializeWasm)
--
-- wasmResult :: WasmResult Response
-- wasmResult = initializeWasm (runPortableHandler defaultState $ processMessage msg)
-- @
module ExoMonad.Wasm.Run
  ( -- * Running Portable Handlers
    runPortableHandler,
    runPortableHandlerNoState,

    -- * Effect Stack Types
    PortableEffects,
    PortableEffectsNoState,
  )
where

import Control.Monad.Freer (Eff)
import Control.Monad.Freer.Coroutine (Yield)
import Data.Aeson (FromJSON, ToJSON)
import ExoMonad.Effect.Types (LLM, Log, State)
import ExoMonad.Wasm.Effect (WasmM)
import ExoMonad.Wasm.Interpreter
  ( runLLMAsYield,
    runLogAsYield,
    runStateAsYield,
  )
import ExoMonad.Wasm.WireTypes (EffectResult, SerializableEffect)

-- | Effect stack for portable handlers with state.
--
-- Handlers can use @Member LLM effs@, @Member Log effs@, and @Member (State s) effs@.
type PortableEffects s =
  '[ LLM,
     Log,
     State s,
     Yield SerializableEffect EffectResult
   ]

-- | Effect stack for portable handlers without state.
--
-- Handlers can use @Member LLM effs@ and @Member Log effs@.
type PortableEffectsNoState =
  '[ LLM,
     Log,
     Yield SerializableEffect EffectResult
   ]

-- | Run a portable handler in WASM context.
--
-- Interprets @LLM@, @Log@, and @State s@ effects as @Yield@ effects
-- that can cross the FFI boundary.
--
-- The @initialState@ is used if no persisted state exists in storage.
--
-- = Example
--
-- @
-- handler :: (Member LLM effs, Member Log effs, Member (State MyState) effs)
--         => Input -> Eff effs Output
-- handler input = ...
--
-- wasmComputation :: Input -> WasmM Output
-- wasmComputation = runPortableHandler defaultMyState . handler
-- @
runPortableHandler ::
  forall s a.
  (ToJSON s, FromJSON s) =>
  -- | Initial state
  s ->
  Eff (PortableEffects s) a ->
  WasmM a
runPortableHandler initialState =
  runStateAsYield initialState
    . runLogAsYield
    . runLLMAsYield

-- | Run a portable handler without state in WASM context.
--
-- Like @runPortableHandler@ but for handlers that don't use @State@.
--
-- = Example
--
-- @
-- handler :: (Member LLM effs, Member Log effs) => Input -> Eff effs Output
-- handler input = ...
--
-- wasmComputation :: Input -> WasmM Output
-- wasmComputation = runPortableHandlerNoState . handler
-- @
runPortableHandlerNoState ::
  Eff PortableEffectsNoState a ->
  WasmM a
runPortableHandlerNoState =
  runLogAsYield
    . runLLMAsYield
