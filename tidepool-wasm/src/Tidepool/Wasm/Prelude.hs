{-# LANGUAGE DataKinds #-}

-- | Graph authoring prelude - one import for common graph definitions.
--
-- = Usage
--
-- Replace your imports:
--
-- @
-- -- Before (12+ lines):
-- import Tidepool.Graph.Types (type (:@), Needs, UsesEffects, Exit, Self)
-- import Tidepool.Graph.Generic (GraphMode(..), type (:-))
-- import qualified Tidepool.Graph.Generic as G (Entry, Exit, LogicNode)
-- import Tidepool.Graph.Goto (Goto, GotoChoice(..), To, gotoChoice, gotoExit, gotoSelf)
-- import Tidepool.Wasm.Effect (WasmM, logInfo, llmComplete)
-- import GHC.Generics (Generic)
-- import Data.Text (Text)
-- import Data.Aeson (FromJSON, ToJSON, Value, object, (.=))
-- import Control.Monad (forM, forM_, when, unless, void)
-- import Data.Maybe (fromMaybe, catMaybes, mapMaybe)
--
-- -- After (1 line):
-- import Tidepool.Wasm.Prelude
-- @
--
-- = What's included
--
-- == Graph Structure
-- * 'Entry', 'Exit', 'LogicNode' - Node type markers
-- * '(:-)' - Mode application (e.g., @mode :- Entry Int@)
-- * '(:@)' - Annotation attachment
-- * 'Needs', 'UsesEffects' - Node annotations
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
module Tidepool.Wasm.Prelude
  ( -- * Graph Structure
    -- ** Node Types
    Entry
  , Exit
  , LogicNode
    -- ** Mode Application
  , GraphMode(..)
  , AsGraph
    -- ** Annotations
  , type (:@)
  , Needs
  , UsesEffects
    -- ** Goto Targets (for UsesEffects)
  , ExitTarget   -- Use: Goto ExitTarget Result (or just use gotoExit at value level)
  , Self

    -- * Transitions
  , Goto
  , GotoChoice   -- Constructors hidden; use gotoChoice/gotoExit/gotoSelf
  , OneOf        -- Constructors hidden; import Goto.Internal for dispatch
  , To
  , gotoChoice
  , gotoExit
  , gotoSelf

    -- * Effects
  , WasmM
  , logInfo
  , logError
  , llmComplete

    -- * Common Types
  , Text
  , Generic
  , FromJSON(..)
  , ToJSON(..)

    -- * JSON (Aeson)
  , Value(..)
  , object
  , (.=)
  , (.:)
  , (.:?)

    -- * Control.Monad
  , forM
  , forM_
  , when
  , unless
  , void
  , (>=>)
  , (<=<)

    -- * Data.Maybe
  , fromMaybe
  , catMaybes
  , mapMaybe
  , isJust
  , isNothing

    -- * Data.Either
  , lefts
  , rights
  , partitionEithers

    -- * Data.Text
  , pack
  , unpack

    -- * Lifted Conditionals
  , whenM
  , unlessM
  ) where

-- Graph structure
import Tidepool.Graph.Types (type (:@), Needs, UsesEffects, Self)
import qualified Tidepool.Graph.Types as Types (Exit)
import Tidepool.Graph.Generic
  ( GraphMode(..)
  , AsGraph
  , type (:-)
  , Entry
  , Exit
  , LogicNode
  )

-- Transitions
import Tidepool.Graph.Goto
  ( Goto
  , GotoChoice
  , OneOf
  , To
  , gotoChoice
  , gotoExit
  , gotoSelf
  )

-- Effects
import Tidepool.Wasm.Effect (WasmM, logInfo, logError, llmComplete)

-- Common types
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), object, (.=), (.:), (.:?))

-- Common combinators
import Control.Monad (forM, forM_, when, unless, void, (>=>), (<=<))
import Data.Maybe (fromMaybe, catMaybes, mapMaybe, isJust, isNothing)
import Data.Either (lefts, rights, partitionEithers)


-- ============================================================================
-- Lifted Conditionals
-- ============================================================================

-- | Lifted 'when' - run action when monadic condition is true.
--
-- @
-- whenM (isReady state) $ logInfo "Ready to proceed"
-- @
whenM :: Monad m => m Bool -> m () -> m ()
whenM mb action = mb >>= \b -> when b action

-- | Lifted 'unless' - run action when monadic condition is false.
--
-- @
-- unlessM (hasErrors state) $ logInfo "Processing complete"
-- @
unlessM :: Monad m => m Bool -> m () -> m ()
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
