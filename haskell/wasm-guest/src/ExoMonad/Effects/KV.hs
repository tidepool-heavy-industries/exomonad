{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Key-value storage effects for persistent agent state.
--
-- All effects are dispatched via the @kv@ namespace.
-- Request and response types are proto-generated from @proto/effects/kv.proto@.
--
-- = Example
--
-- @
-- import ExoMonad.Effects.KV
--
-- main :: IO ()
-- main = do
--   _ <- kvSet (SetRequest "mykey" "myvalue" "")
--   result <- kvGet (GetRequest "mykey" "")
--   pure ()
-- @
module ExoMonad.Effects.KV
  ( -- * Effect Types
    KVGet,
    KVSet,

    -- * Smart Constructors
    kvGet,
    kvSet,

    -- * Re-exported proto types
    module Effects.Kv,
  )
where

import Effects.EffectError (EffectError)
import Effects.Kv
import ExoMonad.Effect.Class (Effect (..), runEffect)

-- ============================================================================
-- Effect phantom types + instances
-- ============================================================================

data KVGet

instance Effect KVGet where
  type Input KVGet = GetRequest
  type Output KVGet = GetResponse
  effectId = "kv.get"

data KVSet

instance Effect KVSet where
  type Input KVSet = SetRequest
  type Output KVSet = SetResponse
  effectId = "kv.set"

-- ============================================================================
-- Smart constructors
-- ============================================================================

kvGet :: GetRequest -> IO (Either EffectError GetResponse)
kvGet = runEffect @KVGet

kvSet :: SetRequest -> IO (Either EffectError SetResponse)
kvSet = runEffect @KVSet
