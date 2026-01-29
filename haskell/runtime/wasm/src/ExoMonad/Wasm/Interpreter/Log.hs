{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

-- | Log effect interpreter for WASM.
--
-- This module provides @runLogAsYield@, which interprets the @Log@ effect
-- by yielding @SerializableEffect@ to TypeScript.
--
-- = Design
--
-- The interpreter converts:
--
-- * @LogMsg Trace msg fields@ → dropped
-- * @LogMsg Debug msg fields@ → dropped (no debug in WASM)
-- * @LogMsg Info msg fields@ → @EffLogInfo msg fields@
-- * @LogMsg Warn msg fields@ → @EffLogInfo msg fields@ (treated as info)
-- * @LogMsg Error msg fields@ → @EffLogError msg fields@
--
-- Fire-and-forget semantics - the result from TypeScript is ignored.
module ExoMonad.Wasm.Interpreter.Log
  ( runLogAsYield
  ) where

import Control.Monad.Freer (Eff, Member, interpret)
import Control.Monad.Freer.Coroutine (Yield, yield)
import qualified Data.Map.Strict as Map

import ExoMonad.Effect.Types (Log(..), LogLevel(..))
import ExoMonad.Wasm.WireTypes
  ( SerializableEffect(..)
  , EffectResult(..)
  )


-- | Interpret @Log@ effect by yielding to TypeScript.
--
-- This interpreter converts @Log@ operations to @Yield SerializableEffect@.
-- TypeScript logs the message and acknowledges (result ignored).
--
-- Log levels are mapped as:
--
-- * 'Trace' - Dropped
-- * 'Debug' - Dropped (not sent to TypeScript)
-- * 'Info' - Sent as @EffLogInfo@
-- * 'Warn' - Sent as @EffLogInfo@ (no separate warn level in wire format)
-- * 'Error' - Sent as @EffLogError@
--
-- Structured fields are preserved for queryable log data.
runLogAsYield
  :: Member (Yield SerializableEffect EffectResult) effs
  => Eff (Log ': effs) a
  -> Eff effs a
runLogAsYield = interpret handleLog
  where
    handleLog :: Member (Yield SerializableEffect EffectResult) effs
              => Log x -> Eff effs x
    handleLog (LogMsg level msg maybeFields) = case level of
      -- Trace and Debug logs are dropped in WASM (too noisy for production)
      Trace -> pure ()
      Debug -> pure ()

      -- Info and Warn both go to EffLogInfo
      Info -> do
        let fields = fmap Map.fromList maybeFields
        _ <- yield (EffLogInfo msg fields) (id @EffectResult)
        pure ()

      Warn -> do
        let fields = fmap Map.fromList maybeFields
        _ <- yield (EffLogInfo msg fields) (id @EffectResult)
        pure ()

      -- Error goes to EffLogError
      Error -> do
        let fields = fmap Map.fromList maybeFields
        _ <- yield (EffLogError msg fields) (id @EffectResult)
        pure ()
