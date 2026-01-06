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
-- * @LogMsg Info msg fields@ → @EffLogInfo msg fields@
-- * @LogMsg Warn msg fields@ → @EffLogInfo msg fields@ (treated as info)
-- * @LogMsg Debug msg fields@ → dropped (no debug in WASM)
--
-- Fire-and-forget semantics - the result from TypeScript is ignored.
module Tidepool.Wasm.Interpreter.Log
  ( runLogAsYield
  ) where

import Control.Monad.Freer (Eff, Member, interpret)
import Control.Monad.Freer.Coroutine (Yield, yield)
import qualified Data.Map.Strict as Map

import Tidepool.Effect.Types (Log(..), LogLevel(..))
import Tidepool.Wasm.WireTypes
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
-- * 'Debug' - Dropped (not sent to TypeScript)
-- * 'Info' - Sent as @EffLogInfo@
-- * 'Warn' - Sent as @EffLogInfo@ (no separate warn level in wire format)
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
      -- Debug logs are dropped in WASM (too noisy for production)
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
