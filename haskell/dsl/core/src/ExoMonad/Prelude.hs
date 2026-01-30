{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module ExoMonad.Prelude
  ( -- * Core types
    Type,
    Constraint,
    Generic,
    Symbol,
    KnownSymbol,
    symbolVal,
    Proxy (..),

    -- * Common data types
    NonEmpty (..),
    ByteString,
    Map,
    HashMap,
    HashSet,
    Set,
    Vector,

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

    -- * ExoMonad Graph DSL
    type (:@),
    Input,
    Schema,
    Template,
    UsesEffects,
    EntryPoint,
    Exit,
    Self,

    -- * Graph Combinators
    Goto,
    goto,
    gotoChoice,
    gotoExit,
    gotoSelf,
    (-->),

    -- * JSON
    Value (..),
    FromJSON (..),
    ToJSON (..),
    object,
    (.=),
    (.:),
    (.:?),

    -- * Maybe
    fromMaybe,
    catMaybes,
    isJust,
    isNothing,
    listToMaybe,
    maybeToList,

    -- * Either
    isLeft,
    isRight,
    lefts,
    rights,

    -- * Control & Flow
    when,
    unless,
    void,
    forM,
    forM_,
    traverse,
    traverse_,
    toList,
    optional,
    Alternative (..),

    -- * Qualified re-exports (import ExoMonad.Prelude gets you T.foo, M.foo, etc.)
    module T,
    module M,
    module BS,
    module V,
    module HM,
    module HS,
    module S,
  )
where

import Control.Applicative (Alternative (..), optional)
import Control.Monad (unless, void, when)
import Control.Monad.Freer (Eff, LastMember, Member, interpret, send, sendM)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.:), (.:?), (.=))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Either (isLeft, isRight, lefts, rights)
import Data.Foldable (forM_, toList, traverse_)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, maybeToList)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable (forM)
import Data.Vector (Vector)
import Data.Vector qualified as V
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
    logWarn,
    modify,
    put,
    randomInt,
    requestChoice,
    requestText,
    returnValue,
  )
import ExoMonad.Graph.Goto (Goto, goto, gotoChoice, gotoExit, gotoSelf, (-->))
import ExoMonad.Graph.Types (EntryPoint, Exit, Input, Schema, Self, Template, UsesEffects, type (:@))
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
