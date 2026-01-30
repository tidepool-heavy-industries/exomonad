{-# LANGUAGE DataKinds #-}

-- | Graph authoring prelude - one import for common graph definitions.
--
-- = Usage
--
-- Replace your imports:
--
-- @
-- -- Before (12+ lines):
-- import ExoMonad.Graph.Types (type (:@), Input, UsesEffects, Exit, Self)
-- import ExoMonad.Graph.Generic (GraphMode(..), type (:-))
-- import qualified ExoMonad.Graph.Generic as G (Entry, Exit, LogicNode)
-- import ExoMonad.Graph.Goto (Goto, GotoChoice(..), To, gotoChoice, gotoExit, gotoSelf)
-- import ExoMonad.Wasm.Effect (WasmM, logInfo, llmComplete)
-- import GHC.Generics (Generic)
-- import Data.Text (Text)
-- import Data.Aeson (FromJSON, ToJSON, Value, object, (.=))
-- import Control.Monad (forM, forM_, when, unless, void)
-- import Data.Maybe (fromMaybe, catMaybes, mapMaybe)
--
-- -- After (1 line):
-- import ExoMonad.Wasm.Prelude
-- @
--
-- = What's included
--
-- == Graph Structure
-- * 'Entry', 'Exit', 'LogicNode' - Node type markers
-- * '(:-)' - Mode application (e.g., @mode :- Entry Int@)
-- * '(:@)' - Annotation attachment
-- * 'Input', 'UsesEffects' - Node annotations
-- * 'GraphMode', 'AsGraph' - Mode types
--
-- == Transitions
-- * 'Goto' - Transition effect
-- * 'GotoChoice', 'To' - Handler return types
-- * 'gotoChoice', 'gotoExit', 'gotoSelf' - Smart constructors
-- * 'OneOf' - Sum type for dispatch
--
-- == Effects
-- * 'WasmM' - Effect monad for WASM handlers
-- * 'logInfo', 'logError' - Logging effects
-- * 'llmComplete' - LLM completion effect
--
-- == Common Types
-- * 'Text' - Text type
-- * 'Generic' - For deriving
-- * 'FromJSON', 'ToJSON' - JSON serialization
-- * 'Value', 'object', '.=', '.:' - Aeson JSON construction
--
-- == Common Combinators
-- * 'forM', 'forM_', 'when', 'unless', 'void' - Control.Monad
-- * 'fromMaybe', 'catMaybes', 'mapMaybe', 'isJust', 'isNothing' - Data.Maybe
-- * 'lefts', 'rights', 'partitionEithers' - Data.Either
-- * 'pack', 'unpack' - Data.Text conversions
-- * 'whenM', 'unlessM' - Lifted monad conditionals
module ExoMonad.Wasm.Prelude
  ( -- * Graph Structure

    -- ** Node Types
    EntryNode,
    ExitNode,
    LogicNode,

    -- ** Mode Application
    GraphMode (..),
    AsGraph,

    -- ** Annotations
    type (:@),
    Input,
    UsesEffects,

    -- ** Goto Targets (for UsesEffects)
    ExitTarget, -- Use: Goto ExitTarget Result (or just use gotoExit at value level)
    Self,

    -- * Transitions
    Goto,
    GotoChoice, -- Constructors hidden; use gotoChoice/gotoExit/gotoSelf
    OneOf, -- Constructors hidden; import Goto.Internal for dispatch
    To,
    gotoChoice,
    gotoExit,
    gotoSelf,

    -- * Effects
    WasmM,
    logInfo,
    logError,
    llmComplete,

    -- * Common Types
    Text,
    Generic,
    FromJSON (..),
    ToJSON (..),

    -- * JSON (Aeson)
    Value (..),
    object,
    (.=),
    (.:),
    (.:?),

    -- * Control.Monad
    forM,
    forM_,
    when,
    unless,
    void,
    (>=>),
    (<=<),

    -- * Data.Maybe
    fromMaybe,
    catMaybes,
    mapMaybe,
    isJust,
    isNothing,

    -- * Data.Either
    lefts,
    rights,
    partitionEithers,

    -- * Data.Text
    pack,
    unpack,

    -- * Lifted Conditionals
    whenM,
    unlessM,
  )
where

-- Graph structure

-- Transitions

-- Effects

-- Common types

-- Common combinators
import Control.Monad (forM, forM_, unless, void, when, (<=<), (>=>))
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.:), (.:?), (.=))
import Data.Either (lefts, partitionEithers, rights)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, mapMaybe)
import Data.Text (Text, pack, unpack)
import ExoMonad.Graph.Generic
  ( AsGraph,
    EntryNode,
    ExitNode,
    GraphMode (..),
    LogicNode,
    type (:-),
  )
import ExoMonad.Graph.Goto
  ( Goto,
    GotoChoice,
    OneOf,
    To,
    gotoChoice,
    gotoExit,
    gotoSelf,
  )
import ExoMonad.Graph.Types (Input, Self, UsesEffects, type (:@))
import ExoMonad.Graph.Types qualified as Types (Exit)
import ExoMonad.Wasm.Effect (WasmM, llmComplete, logError, logInfo)
import GHC.Generics (Generic)

-- ============================================================================
-- Lifted Conditionals
-- ============================================================================

-- | Lifted 'when' - run action when monadic condition is true.
--
-- @
-- whenM (isReady state) $ logInfo "Ready to proceed"
-- @
whenM :: (Monad m) => m Bool -> m () -> m ()
whenM mb action = mb >>= \b -> when b action

-- | Lifted 'unless' - run action when monadic condition is false.
--
-- @
-- unlessM (hasErrors state) $ logInfo "Processing complete"
-- @
unlessM :: (Monad m) => m Bool -> m () -> m ()
unlessM mb action = mb >>= \b -> unless b action

-- | Type alias for Exit as a Goto target.
--
-- Use in UsesEffects annotations:
--
-- @
-- :@ UsesEffects '[Goto ExitTarget Result]
-- @
--
-- At value level, just use 'gotoExit' instead.
type ExitTarget = Types.Exit
