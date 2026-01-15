module Tidepool.Prelude
  ( -- * Core types
    Text
  , Type
  , Constraint
  , Generic
  , Symbol
  , KnownSymbol
  , symbolVal
  , Proxy(..)

    -- * Effect system
  , Eff
  , Member
  , send
  , interpret
  , sendM
  , LastMember

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

    -- * Maps
  , Map

    -- * Control
  , when
  , unless
  , void

    -- * Qualified re-exports (import Tidepool.Prelude gets you T.foo and M.foo)
  , module T
  , module M
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Freer (Eff, Member, send, interpret, sendM, LastMember)
import Data.Kind (Type, Constraint)
import Data.Aeson (Value(..), FromJSON(..), ToJSON(..), object, (.=), (.:), (.:?))
import GHC.Generics (Generic)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Maybe (fromMaybe, catMaybes, isJust, isNothing, listToMaybe, maybeToList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad (when, unless, void)
