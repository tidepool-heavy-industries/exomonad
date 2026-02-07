{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Type-safe effect dispatch via the Effect typeclass.
--
-- This module provides a strongly-typed interface for invoking effects.
-- Each effect type has associated Input and Output types, and an effect ID
-- that routes to the appropriate handler on the Rust side.
--
-- Request and response types must have proto3-suite 'Message' instances
-- (generated from .proto files by compile-proto-file).
--
-- = Architecture
--
-- @
-- runEffect @GitGetBranch req
--     │
--     │ effectId @GitGetBranch = "git.get_branch"
--     ▼
-- yieldEffect "git.get_branch" (encode req)
--     │  EffectEnvelope { effect_type, payload } (protobuf binary)
--     ▼
-- Rust EffectRegistry
--     └── "git" → GitHandler
--             └── "get_branch" → GitHandler::get_branch
-- @
--
-- = Usage
--
-- @
-- -- Use the smart constructor
-- result <- getBranch (GetBranchRequest { getBranchRequestWorkingDir = "." })
--
-- -- Or use runEffect directly with type application
-- result <- runEffect @GitGetBranch req
-- @
module ExoMonad.Effect.Class
  ( -- * Core Typeclass
    Effect (..),

    -- * Running Effects
    runEffect,
    runEffect_,

    -- * Re-exports
    EffectError (..),
    EffectErrorKind (..),
  )
where

import Data.Kind (Type)
import Data.Text.Lazy (Text)
import ExoMonad.Guest.Effect (EffectError (..), EffectErrorKind (..), yieldEffect, yieldEffect_)
import Proto3.Suite.Class (Message)

-- | Typeclass for typed effects.
--
-- Each effect is represented by a phantom type that carries:
-- - The input type (request) — must be a proto3-suite 'Message'
-- - The output type (response) — must be a proto3-suite 'Message'
-- - The effect identifier for routing
--
-- = Example
--
-- @
-- -- Define an effect phantom type
-- data GitGetBranch
--
-- -- Provide the instance
-- instance Effect GitGetBranch where
--   type Input GitGetBranch = GetBranchRequest
--   type Output GitGetBranch = GetBranchResponse
--   effectId = "git.get_branch"
-- @
class Effect (e :: Type) where
  -- | The request type for this effect (proto3-suite Message).
  type Input e :: Type

  -- | The response type for this effect (proto3-suite Message).
  type Output e :: Type

  -- | The effect identifier (namespace.effect_name).
  -- Must match the Rust handler's namespace and effect name.
  effectId :: Text

-- | Run a typed effect.
--
-- This is the primary way to invoke effects with full type safety.
-- The effect ID is derived from the Effect instance.
--
-- @
-- result <- runEffect @GitGetBranch (GetBranchRequest ".")
-- case result of
--   Left err -> handleError err
--   Right resp -> use (getBranchResponseBranch resp)
-- @
runEffect ::
  forall e.
  ( Effect e,
    Message (Input e),
    Message (Output e)
  ) =>
  Input e ->
  IO (Either EffectError (Output e))
runEffect input = yieldEffect (effectId @e) input

-- | Run a typed effect that returns no meaningful response.
--
-- Use this for fire-and-forget effects where you only care about success/failure.
--
-- @
-- result <- runEffect_ @LogInfo (InfoRequest "Starting process" Nothing)
-- @
runEffect_ ::
  forall e.
  ( Effect e,
    Message (Input e)
  ) =>
  Input e ->
  IO (Either EffectError ())
runEffect_ input = yieldEffect_ (effectId @e) input
