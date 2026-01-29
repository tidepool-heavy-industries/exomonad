module ExoMonad.Prelude
  ( -- * Core types
    Type
  , Constraint
  , Generic
  , Symbol
  , KnownSymbol
  , symbolVal
  , Proxy(..)

    -- * Common data types
  , NonEmpty(..)

    -- * Effect system
  , Eff
  , Member
  , send
  , interpret
  , sendM
  , LastMember

    -- * ExoMonad Effects
  , LLM
  , State, get, put, modify, gets
  , Log, logInfo, logError, logDebug, logWarn
  , Emit, emit
  , RequestInput, requestText, requestChoice
  , Time, getCurrentTime
  , Random, randomInt
  , Return, returnValue

    -- * ExoMonad Graph DSL
  , type (:@)
  , Input
  , Schema
  , Template
  , UsesEffects
  , EntryPoint
  , Exit
  , Self

    -- * Graph Combinators
  , Goto, goto
  , gotoChoice, gotoExit, gotoSelf
  , (-->)

    -- * JSON
  , Value(..)
  , FromJSON(..)
  , ToJSON(..)
  , object
  , (.=)
  , (.:)
  , (.:?)

    -- * Maybe
  , fromMaybe
  , catMaybes
  , isJust
  , isNothing
  , listToMaybe
  , maybeToList

    -- * Either
  , isLeft
  , isRight
  , lefts
  , rights

    -- * Control & Flow
  , when
  , unless
  , void
  , forM
  , forM_
  , traverse
  , traverse_
  , toList
  , optional
  , Alternative(..)

    -- * Qualified re-exports (import ExoMonad.Prelude gets you T.foo, M.foo, etc.)
  , module T
  , module M
  , module BS
  , module V
  , module HM
  , module HS
  , module S
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Set (Set)
import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.List.NonEmpty (NonEmpty(..))

import Control.Monad.Freer (Eff, Member, send, interpret, sendM, LastMember)
import Data.Kind (Type, Constraint)
import Data.Aeson (Value(..), FromJSON(..), ToJSON(..), object, (.=), (.:), (.:?))
import GHC.Generics (Generic)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Maybe (fromMaybe, catMaybes, isJust, isNothing, listToMaybe, maybeToList)
import Control.Monad (when, unless, void)
import Data.Foldable (forM_, traverse_, toList)
import Data.Traversable (forM)
import Data.Either (isLeft, isRight, lefts, rights)
import Control.Applicative (Alternative(..), optional)

import ExoMonad.Effect.Types
  ( LLM, State, get, put, modify, gets
  , Log, logInfo, logError, logDebug, logWarn
  , Emit, emit
  , RequestInput, requestText, requestChoice
  , Time, getCurrentTime
  , Random, randomInt
  , Return, returnValue
  )
import ExoMonad.Graph.Types (type (:@), Input, Schema, Template, UsesEffects, EntryPoint, Exit, Self)
import ExoMonad.Graph.Goto (Goto, goto, gotoChoice, gotoExit, gotoSelf, (-->))
