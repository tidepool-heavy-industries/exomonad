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
-- import Data.Aeson (FromJSON, ToJSON)
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
module Tidepool.Wasm.Prelude
  ( -- * Graph Structure
    -- ** Node Types
    Entry
  , Exit
  , LogicNode
    -- ** Mode Application
  , type (:-)
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
  , GotoChoice(..)
  , OneOf(..)
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
  , GotoChoice(..)
  , OneOf(..)
  , To
  , gotoChoice
  , gotoExit
  , gotoSelf
  )

-- Effects
import Tidepool.Wasm.Effect (WasmM, logInfo, logError, llmComplete)

-- Common types
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..), ToJSON(..))

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
