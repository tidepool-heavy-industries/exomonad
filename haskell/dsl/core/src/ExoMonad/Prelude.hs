{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module ExoMonad.Prelude
  ( -- * Relude (Standard Library Replacement)
    module Prelude,

    -- * Effect system
    Eff,
    Member,
    send,
    interpret,
    sendM,
    LastMember,

    -- * ExoMonad Effects
    LLM,
    State,
    get,
    put,
    modify,
    gets,
    Log,
    logInfo,
    logError,
    logDebug,
    logWarn,
    logTrace,
    Emit,
    emit,
    RequestInput,
    requestText,
    requestChoice,
    Time,
    getCurrentTime,
    Random,
    randomInt,
    Return,
    returnValue,

    -- * JSON
    Value (..),
    FromJSON (..),
    ToJSON (..),
    object,
    (.=),
    (.:),
    (.:?),

    -- * Qualified re-exports
    module T,
    module M,
    module BS,
    module V,
    module HM,
    module HS,
    module S,
  )
where

-- Relude re-exports
import Prelude hiding
  ( -- Hide things that collide with ExoMonad DSL or are legacy
    id,
    trace,
    -- We use Polysemy's State/Reader usually, but Relude's are fine if qualified.
    -- However, we export our own 'State' effect.
    State,
    get,
    put,
    modify,
    gets,
  )

-- Effect System
import Polysemy (Sem, Member, interpret, send, embed, makeSem)
import Polysemy.Embed (Embed)

-- | Alias for Polysemy's 'Sem' to maintain compatibility
type Eff = Sem

-- | Compatibility alias for @'Member' ('Embed' m)@ to mimic the traditional
--   @LastMember@ constraint from earlier versions.
--
--   This is a temporary shim to preserve the old API surface while the
--   codebase migrates to using @Member (Embed m)@ directly. New code
--   should prefer the more explicit @Member (Embed m) r@ constraint.
--   This alias may be deprecated and removed in a future major release.
type LastMember m r = Member (Embed m) r

-- | Compatibility alias for 'embed' to mimic the traditional @sendM@ helper
--   from earlier versions.
--
--   This is a temporary shim to preserve the old API surface while the
--   codebase migrates to using 'embed' directly. New code should prefer
--   calling 'embed' rather than 'sendM'. This alias may be deprecated and
--   removed in a future major release.
sendM :: Member (Embed m) r => m a -> Sem r a
sendM = embed

-- JSON
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.:), (.:?), (.=))

-- Qualified Imports (Standard)
import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V

-- ExoMonad Internal Imports
import ExoMonad.Effect.Types
  ( Emit,
    LLM,
    Log,
    Random,
    RequestInput,
    Return,
    State,
    Time,
    emit,
    get,
    getCurrentTime,
    gets,
    logDebug,
    logError,
    logInfo,
    logTrace,
    logWarn,
    modify,
    put,
    randomInt,
    requestChoice,
    requestText,
    returnValue,
  )