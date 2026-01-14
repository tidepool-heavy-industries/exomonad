module Tidepool.Prelude
  ( -- * Core types
    Text
  , Type
  , Constraint
  , Generic
  , Symbol
  , KnownSymbol
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
  , toJSON
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
  ) where

import Data.Text (Text)
import Control.Monad.Freer (Eff, Member, send, interpret, sendM, LastMember)
import Data.Kind (Type, Constraint)
import Data.Aeson (Value(..), toJSON, FromJSON(..), ToJSON(..), object, (.=), (.:), (.:?))
import GHC.Generics (Generic)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (Symbol, KnownSymbol)
import Data.Maybe (fromMaybe, catMaybes, isJust, isNothing)
