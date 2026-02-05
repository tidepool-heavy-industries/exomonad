{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module ExoMonad.Prelude
  ( -- * Relude (Standard Library Replacement)
    module Prelude,

    -- * Effect system
    Eff,
    Member,
    send,
    interpret,

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
    emitEvent,
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

-- Hide things that collide with ExoMonad DSL or are legacy

-- We use Polysemy's State/Reader usually, but Relude's are fine if qualified.
-- However, we export our own 'State' effect.

-- Effect System

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
    emitEvent,
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
import Polysemy (Member, Sem, embed, interpret, makeSem, send)
import Polysemy.Embed (Embed)
import Prelude hiding
  ( State,
    get,
    gets,
    id,
    modify,
    put,
    trace,
  )

-- | Alias for Polysemy's 'Sem' to maintain compatibility
type Eff = Sem
